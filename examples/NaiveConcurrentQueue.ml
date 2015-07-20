
module type S = sig
  type ('a, 'b) reagent

  type 'a t
  val create : unit -> 'a t
  val push : 'a t -> ('a, unit) reagent
  val pop : 'a t -> (unit, 'a option) reagent
  val pop_until : 'a t -> ('a -> bool) -> (unit, unit) reagent
  type 'a cursor
  val snapshot : 'a t -> 'a cursor
  val next : 'a cursor -> ('a * 'a cursor) option
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open CAS.Sugar
  open Reagent.Sugar

  (* Concurrent queue based on functionnal queue stored in a CAS reference.
   * Not the most efficient concurrent queue, but has avantages, like returning
   * the whole content without copying it! This queue is non-blocking, it only
   * fail trasiently.
   *)

  type 'a t = 'a FQueue.t casref

  let create () = ref FQueue.empty

  let push q =
    Reagent.computed (fun v ->
      let s = !q in Reagent.cas q (s --> FQueue.push v s))

  let pop q =
    Reagent.computed (fun () ->
      try
        let s = !q in
        let (v, n) = FQueue.pop s in
        Reagent.cas q (s --> n) >>>
        Reagent.constant (Some v)
      with FQueue.Empty -> Reagent.constant None)

  let pop_until q f =
    let rec loop fq =
      try
        let (v, n) = FQueue.pop fq in
        if not (f v) then loop n
        else fq
      with FQueue.Empty -> fq
    in
    Reagent.computed (fun () ->
      let s = !q in Reagent.cas q (s --> loop s))

  type 'a cursor = 'a FQueue.t

  let snapshot q = !q

  let next c = try Some (FQueue.pop c) with FQueue.Empty -> None

end
