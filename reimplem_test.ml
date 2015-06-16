
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
  | _ -> continue k (perform x)

let h1 (type u) (x:u eff) (k:(u, 'b) cont) : 'b = match x with
  | OfString s -> continue k (int_of_string s)
  | _ -> continue k (perform x)

let h2 (type u) (x:u eff) (k:(u, 'b) cont) : 'b = match x with
  | OfInt i -> continue k i
  | _ -> continue k (perform x)

let _ =
  (* That print 7, which seems to be allright *)
  print "First  %d\n" begin
          tryeff f { handle = h }
        end ;
  flush_all () ;
  (* That print 7, but seems to be non deterministic about termination... Humm... *)
  print "Second %d\n" begin
          tryeff (fun () -> tryeff f { handle = h1 })
                 { handle = h2 }
        end ;
