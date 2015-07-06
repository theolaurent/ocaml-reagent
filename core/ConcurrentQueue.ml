
open CAS.Sugar
open Reagent.Sugar

type 'a node = N of ('a * 'a node) option casref

type 'a t = { head : 'a node ; tail : 'a node }
(* invariant : head = None <=> tail = None          *)
(* invariant : tail = Some (x, node) => node = None *)

let create () = { head = N (ref None) ; tail = N (ref None) }

let push q =
  Reagent.computed (fun v ->
    let newnode = Some (v, N (ref None)) in
    let N head = q.head in
    let N tail = q.tail in
    let s = !(tail) in
    match s with
    | None             -> Reagent.cas head (None --> newnode)
                      >>> Reagent.cas tail (None --> newnode)
    | Some (_, N node) -> assert (!node = None) ;
                          Reagent.cas tail (s    --> newnode)
                      >>> Reagent.cas node (None --> newnode)
  )

(* TODO: I feel this pop reagent could be written in a more elegant way *)
let pop q =
  Reagent.computed (fun () ->
    let N head = q.head in
    let N tail = q.tail in
    let s = !(head) in
    match s with
    | None -> Reagent.retry (* non blocking queue *)
    | Some (v, N node) ->
       let s' = !node in
       begin match s' with
             | None   -> Reagent.cas head (s --> None)
                     >>> Reagent.cas tail (s --> None)
                     >>> Reagent.constant v
             | Some _ -> Reagent.cas head (s --> s'  )
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

let snapshot q = q.head

let next (N c) = !c
