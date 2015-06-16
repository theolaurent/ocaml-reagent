
(*
 * A reimplementation of the effect system with bare ocaml 4.02,
 * using threads to get a kind of duplication of the stack.
 *
 * The Mvar module has been borrowed here : https://github.com/johnelse/ocaml-mvar.
 *
 * Usage :
 * let h (type u) (x:u eff) (k:(u, 'b) cont) : 'b = match x with
 *   | Effect -> [... some code ...]
 *   | _ -> continue k (perform x)
 *
 * tryeff (function () -> ... [some code] ... perform Effect ... [some code] ...)
 *        { handle = h }
 *
 * TODO : My "continuations" are clearly one-shot, but there is no second-call detection,
 * that might create problems.
 *
 * TODO : I think there might be a problem with multiple threads on user-side.
 *  If a user use this library
 * and multiple threads, I think the sharing of the stack might be a problem.
 *
 *)


type _ eff = ..

exception Unhandled


type ('a, 'b) cont = { eff_return : 'a Mvar.t ; resume_waiting : unit -> 'b }

let continue { eff_return ; resume_waiting } x =
    Mvar.put eff_return x ; resume_waiting ()

module type effect = sig
  type t
  val e : t eff
  val mvar_ret : t Mvar.t
  val thread : Thread.t
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
    let thread = Thread.self ()
  end in
  try
    let mv = Stack.pop stack in
    let () = Mvar.put mv (Some (module M : effect)) in
    let res = Mvar.take M.mvar_ret in
    let () = Stack.push mv stack (* is it the right place ? *) in
    res

  with _ -> raise Unhandled

type 'b handler = { handle : 'a . 'a eff -> ('a, 'b) cont -> 'b }


let tryeff (f : unit -> 'b) (h : 'b handler) : 'b =
  let mv = Mvar.create_empty () in
  let () = Stack.push mv stack in
  let t = Resthread.create (fun () -> let r = f () in
                                      let () = Mvar.put mv None in
                                      r) ()
  in
  let rec loop () = match Mvar.take mv with
    | None -> Resthread.get_result t
    | Some e -> let module M : effect = (val e) in
                let res = h.handle M.e { eff_return = M.mvar_ret ;
                                         resume_waiting = loop } in
                (* destroying the "continuation" (in case it has not been consumed) *)
                (* let () = Thread.kill M.thread in *)
                (* but Invalid_argument("Thread.kill: not implemented") *)
                res

  in loop ()
