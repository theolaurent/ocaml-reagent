
open Reimplem

let print = Printf.printf

(* A fisrt test *)

type _ eff += OfString : string -> int eff
type _ eff += OfInt : int -> int eff


let f perform =
  1 + perform (OfString "2") + perform (OfInt 4)

(* That print 7, which seems to be allright *)
let _ =
  print "%d" begin
          tryeff f
                 (fun e k -> match e with
                             | OfInt i -> continue k i
                             | OfString s -> continue k (int_of_string s)
                             | _ -> raise Unhandled)
        end
