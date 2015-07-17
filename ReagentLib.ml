

module type S = sig
  module Reagent : Reagent.S
  module Channel : Channel.S with type ('a, 'b) reagent = ('a, 'b) Reagent.t
  (* module MSQueue: MSQueue.S with type ('a, 'b) reagent = ('a, 'b) Reagent.t *)
end

module Make (Sched : Scheduler.S) : S = struct
  module Reagent = Reagent.Make (Sched)
  module Channel = Channel.Make (Sched)
  (* module MSQueue = MSQueue.Make (Sched) *)
end
