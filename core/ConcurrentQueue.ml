
open CAS.Sugar
open Reagent.Sugar

(* Concurrent queue based on functionnal queue stored in a CAS reference. *)
(* Not the most efficient concurrent queue, but has avantages, like       *)
(* returning the whole content without copying it!                        *)


type 'a t = 'a FQueue.t casref


let create () = ref FQueue.empty

let whole q = !q

let push q =
  Reagent.computed (fun v -> let s = !q in
                             Reagent.cas q (s --> FQueue.push v s))

let pop q =
  Reagent.computed (fun () -> try
                                let s = !q in
                                let (v, n) = FQueue.pop s in
                                Reagent.cas q (s --> n)
                                |> Reagent.constant v
                              with FQueue.Empty ->
                                failwith "MessageQueue.pop: \
                                          No transient failure for now.")

let rec pop_until q f =
  top q |> Reagent.computed (fun v -> if f v then Reagent.constant v
                                      else pop q |> Reagent.lift ignore |> pop_until q f)