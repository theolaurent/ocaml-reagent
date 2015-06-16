
open Effects


let print = Printf.printf

(* A fisrt test *)

type _ eff += OfString : string -> int eff
type _ eff += OfInt : int -> int eff

let f () =
  1 + perform (OfString "2") + perform (OfInt 4)

let h1 (type u) (x:u eff) (k:(u, 'b) continuation) : 'b = match x with
  | OfString s -> continue k (int_of_string s)
  | _ -> continue k (perform x)

let h2 (type u) (x:u eff) (k:(u, 'b) continuation) : 'b = match x with
  | OfInt i -> continue k i
  | _ -> continue k (perform x)


let _ =
  (* That print 7, which seems to be allright *)
  print "Result: %d\n" begin
          handle { eff = h1 ;
                  exn = raise ;
                  return = (fun x -> x) }
                (fun () ->
                 handle { eff = h2 ;
                          exn = raise ;
                          return = (fun x -> x) }
                        f ()
                ) ()
        end ;
