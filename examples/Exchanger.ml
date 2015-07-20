module type S = sig
  type ('a, 'b) reagent

  type 'a t
  val create : unit -> 'a t
  val exchange : 'a t -> ('a, 'a) reagent
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  module Channel = Channel.Make (Sched) (* TODO: should use ReagentLib.Make? *)

  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open Reagent.Sugar

  type 'a t = (('a, 'a) Channel.endpoint * ('a, 'a) Channel.endpoint)

  let create = Channel.create

  (* TODO: it would be better with a symmetric choose? *)
  let exchange e = Channel.swap (fst e) >+> Channel.swap (snd e)

end
