(* tests *)
open ParamShiftReset
(*  (reset ( * 2 (shift k (k (k 4))))) *)
let w = make_wit ()
let _ = reset w (fun () ->
          2 * (shift w (fun k -> k (k 4)))
                )
let _ = reset w (fun () ->
          2 * (shift w (fun k -> k 4))
                )

(*  (reset
   (begin
     (shift k (cons 1 (k (void)))) ;; (1)
     null)) *)
let w = make_wit ()
let _ = reset w (fun () ->
               begin
                 shift w (fun k -> 1 :: k ()) ;
                 []
               end)


open MonoShiftReset
let x = create ()
let _ = x.reset (fun () ->
          2 * (x.shift (fun k -> k (k 4)))
                )

let _ = x.reset (fun () ->
          2 * (x.shift (fun k -> k 4))
                )



let x = create ()
let _ = x.reset (fun () ->
               begin
                 x.shift (fun k -> 1 :: k ()) ;
                 []
               end)

module SR1 = FunctShiftReset (struct type t = int end)
open SR1

let _ = reset (fun () ->
          2 * (shift (fun k -> k (k 4)))
                )

let _ = reset (fun () ->
          2 * (shift (fun k -> k 4))
                )


module SR2 = FunctShiftReset (struct type t = int list end)
open SR2

let _ = reset (fun () ->
               begin
                 shift (fun k -> 1 :: k ()) ;
                 []
               end)


module M = MonoShiftReset
let x = M.create ()
let y = M.create ()
let _ =
  (x.M.reset (fun () ->
    (y.M.reset (fun () ->
      begin x.M.shift (fun k -> 1 :: k ()) ; 0 end
             )) :: []))
