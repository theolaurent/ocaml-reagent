(*
 * A first try in reimplementing the effect system with
 * bare ocaml 4.02 using threads to get a kind of duplication of the stack.
 * The Mvar module has been borrowed here : https://github.com/johnelse/ocaml-mvar.
 *
 * Usage :
 * tryeff (function perform -> ... [some code] ... perform Effect ... [some code] ...)
          (function Effect k -> ... [the handler] ...
                  | e -> raise Unhandled [or] perform e if there is any perfom function available
 *)

type _ eff = ..

exception Unhandled

type ('a, 'b) cont = { eff_return : 'a Mvar.t ; resume_waiting : unit -> 'b }

let continue { eff_return ; resume_waiting } x =
    Mvar.put eff_return x ; resume_waiting ()

type ('a, 'b) ret_or_eff =
  | Ret of 'a
  | Eff of 'b eff


let tryeff (f : ('a eff -> 'a) -> 'b) (handle : 'a eff -> ('a, 'b) cont -> 'b) : 'b =
  let eff_param = Mvar.create_empty () in
  let eff_return = Mvar.create_empty () in
  let perform e =
    let () = Mvar.put eff_param (Eff e) in
    Mvar.take eff_return
  in
  (* try f *)
  let _ = Thread.create
            (fun x -> let r = f x in Mvar.put eff_param (Ret r))
            perform in
  let rec loop () = match Mvar.take eff_param with
    | Ret x -> x
    | Eff e -> handle e { eff_return ; resume_waiting = loop }
  in loop ()
