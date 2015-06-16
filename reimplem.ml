
(*
 * A second version of the reimplementation of the effect system with
 * bare ocaml 4.02 (using threads to get a kind of duplication of the stack).
 *
 * The Mvar module has been borrowed here : https://github.com/johnelse/ocaml-mvar.
 *
 * Usage :
 * tryeff (function () -> ... [some code] ... perform Effect ... [some code] ...)
          (function Effect k -> ... [the handler] ...
                  | e -> raise Unhandled [or] perform e [if there is any perfom function available]
 *
 * I am just not very shure about what would happen with several handlers interacting
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
end
type effect = (module effect)

let eff_param : effect option Mvar.t = Mvar.create_empty ()

let perform (type u) (a:u eff) =
  let module M = struct
    type t = u
    let e = a
    let mvar_ret =  Mvar.create_empty ()
  end in
  let () = Mvar.put eff_param (Some (module M : effect)) in
  Mvar.take M.mvar_ret

type 'b handler = { handle : 'a . 'a eff -> ('a, 'b) cont -> 'b }


let tryeff (f : unit -> 'b) (h : 'b handler) : 'b =
  let t = Resthread.create (fun () -> let r = f () in
                                      let () = Mvar.put eff_param None in
                                      r) ()
  in
  let rec loop () = match Mvar.take eff_param with
    | None -> Resthread.get_result t
    | Some e -> let module M : effect = (val e) in
                h.handle M.e { eff_return = M.mvar_ret ; resume_waiting = loop }
  in loop ()
