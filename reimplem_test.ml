
open Reimplem


let print = Printf.printf

(* A fisrt test *)

type _ eff += OfString : string -> int eff
type _ eff += OfInt : int -> int eff


let f () =
  1 + perform (OfString "2") + perform (OfInt 4)

let h (type u) (x:u eff) (k:(u, 'b) cont) : 'b = match x with
  | OfInt i -> continue k i
  | OfString s -> continue k (int_of_string s)
  | _ -> raise Unhandled

(* That print 7, which seems to be allright *)
let _ =
  print "%d" begin
          tryeff f
                 { handle = h }
        end
