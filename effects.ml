
(*
 * A reimplementation of the effect system with bare ocaml 4.02,
 * using threads to get a kind of duplication of the stack.
 *
 * The Mvar module has been borrowed here : https://github.com/johnelse/ocaml-mvar.
 *
 * This module interface is identical to the one of the effects module of
 * ocaml multicore. It only misses the syntactic sugar !
 *
 * TODO : I think there might be a problem with multiple threads on user-side.
 *  If a user use this library
 * and multiple threads, I think the sharing of the stack might be a problem.
 *
 *)


type _ eff = ..

exception Unhandled


type ('a, 'b) continuation = { mutable eff_return     : [`Exn of exn | `Res of 'a] Mvar.t option ;
                                       thread         : Thread.t         ;
                                       resume_waiting : unit -> 'b       }

let pass_to_continuation { eff_return ; resume_waiting } x =
  begin match eff_return with
 (* Ensuring that the "continuation" is called only once.  *
  * Using the mvar twice would lead to a blocking          *
  * as its content is taken only once.                     *
  * Maybe it would be more elgant to use a specialized     *
  * data strucure, instead of an mvar...                   *)
  | None -> raise (Invalid_argument "continuation already used")
  | Some mvar -> Mvar.put mvar x ; resume_waiting ()
  end

let continue c x = pass_to_continuation c (`Res x)
let discontinue c e = pass_to_continuation c (`Exn e)

module type effect = sig
  type t
  val e : t eff
  val mvar_ret : [`Res of t | `Exn of exn] Mvar.t (* the one-shot thing should in fact *
                           * be enforced here !                *)
end
type effect = (module effect)

(* stack of mvar for effect to be trnasmitted *)
(* should be protected by a mutex ? *)
let stack : effect option Mvar.t Stack.t = Stack.create ()

let perform (type u) (a:u eff) =
  let module M = struct
    type t = u
    let e = a
    let mvar_ret =  Mvar.create_empty ()
  end in
  try
    let mv = Stack.pop stack     (* is it the right place ? *) in
    let () = Mvar.put mv (Some (module M : effect)) in
    let res = Mvar.take M.mvar_ret in
    let () = Stack.push mv stack (* is it the right place ? *) in
    match res with
    | `Res x -> x
    | `Exn e -> raise e

  with _ -> raise Unhandled

type ('b, 'c) handler = { eff    : 'a . 'a eff -> ('a, 'c) continuation -> 'c ;
                          exn    : exn -> 'c                                  ;
                          return : 'b -> 'c                                   }


let handle h f x =
  let mv = Mvar.create_empty () in
  let () = Stack.push mv stack in
  let t = ThreadWithRes.spawn (fun () -> let r = f x in
                                      let () = Mvar.put mv None in
                                      r) ()
  in
  let rec loop () = match Mvar.take mv with
    | None -> begin match ThreadWithRes.get_result t with
                    | `Res x -> h.return x
                    | `Exn e -> h.exn e
                    | `BrutalFailure -> failwith "One thread failed very brutally"
              end
    | Some e -> let module M : effect = (val e) in
                (* creating the continuation *)
                let c = { eff_return     = Some M.mvar_ret ;
                          thread         = ThreadWithRes.get_thread t  ;
                          resume_waiting = loop            } in
                let res = h.eff M.e c in
                (* destroying the "continuation" (in case it has not been consumed) *)
                (* let () = Thread.kill c.thread in *)
                (* Hmmm... I get Invalid_argument("Thread.kill: not implemented") *)
                res

  in try
    let res = loop () in
    let _ = Stack.pop stack in
    res
  with e -> let _ = Stack.pop stack in raise e


let delegate e k =
  match perform e with
  | v -> continue k v
  | exception e -> discontinue k e
