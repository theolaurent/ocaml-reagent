
open CAS.Sugar
open Reagent.Sugar

type 'a node =
  | Nil
  | Next of 'a * 'a node casref

type 'a t = { head : 'a node casref ; tail : 'a node casref }
(* invariant : head = Nil <=> tail = Nil           *)
(* invariant : tail = Next (x, node) => node = Nil *)

let create () = { head = ref Nil ; tail = ref Nil }

let push q =
  Reagent.computed (fun v ->
    let newnode = Next (v, ref Nil) in
    let s = !(q.tail) in
    match s with
    | Nil            -> Reagent.cas q.head (Nil --> newnode)
                    >>> Reagent.cas q.tail (Nil --> newnode)
    | Next (_, node) -> assert (!node = Nil) ;
                        Reagent.cas q.tail (s   --> newnode)
                    >>> Reagent.cas node   (Nil --> newnode)
  )

(* TODO: I feel this pop reagent could be written in a more elegant way *)
let pop q =
  Reagent.computed (fun () ->
    let s = !(q.head) in
    match s with
    | Nil -> Reagent.retry (* non blocking queue *)
    | Next (v, node) ->
       let s' = !node in
       begin match s' with
             | Nil    -> Reagent.cas q.head (s --> Nil)
                     >>> Reagent.cas q.tail (s --> Nil)
                     >>> Reagent.constant v
             | Next _ -> Reagent.cas q.head (s --> s'  )
                     >>> Reagent.constant v
       end
  )

let rec pop_until q f =
  (  pop q >>> Reagent.computed (fun x ->
                 if f x then Reagent.never
                 else Reagent.post_commit (Reagent.run (pop_until q f))
                 (* use post commit to pop 1-by-1, more efficient that kCAS *)
               )
  ) >+> Reagent.constant ()

type 'a cursor = 'a node

let snapshot q = !(q.head)

let next c = match c with
  | Nil -> None
  | Next (a, n) -> Some (a, !n)
