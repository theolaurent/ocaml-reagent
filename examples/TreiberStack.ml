
module type S = sig
  type ('a, 'b) reagent

  type 'a t
  val push : 'a t -> ('a, unit) reagent
  val tryPop : 'a t -> (unit, 'a option) reagent
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open CAS.Sugar
  open Reagent.Sugar

  type 'a t = { head : 'a list casref }

  let push stack =
    Reagent.computed (fun v ->
      let s = !(stack.head) in
      Reagent.cas stack.head (s --> (v :: s))
    )

  (* TODO: should use update reagent? *)
  let tryPop stack =
    Reagent.computed (fun () ->
      let s = !(stack.head) in match s with
        | [] -> Reagent.constant None
        | h :: t -> Reagent.cas stack.head (s --> t)
                >>> Reagent.constant (Some h)
    )

end
