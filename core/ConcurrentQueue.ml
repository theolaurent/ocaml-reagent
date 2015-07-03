
open CAS.Sugar
open Reagent.Sugar

(* Concurrent queue based on functionnal queue stored in a CAS reference. *)
(* Not the most efficient concurrent queue, but has avantages, like       *)
(* returning the whole content without copying it!                        *)
(* This queue is non-blocking, it only fail trasiently.                   *)


type 'a t = 'a FQueue.t casref


let create () = ref FQueue.empty

let fold f q = FQueue.fold_right f !q

let push q =
  Reagent.computed (fun v -> let s = !q in
                             Reagent.cas q (s --> FQueue.push v s))

let pop q =
  Reagent.computed (fun () -> try
                                let s = !q in
                                let (v, n) = FQueue.pop s in
                                Reagent.cas q (s --> n)
                                |> Reagent.constant v
                              with FQueue.Empty -> Reagent.never)
                                (* TODO: transient fail                     *)
                                (* failwith "MessageQueue.pop: \            *)
                                (*           No transient failure for now." *)

let pop_until q f =
  let rec loop fq =
    try
      let (v, n) = FQueue.pop fq in
      if not (f v) then loop n
      else fq
    with FQueue.Empty -> fq
  in
  Reagent.computed (fun () -> let s = !q in
                              Reagent.cas q (s --> loop s))
