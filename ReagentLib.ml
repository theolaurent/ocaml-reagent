
module Make (Sched : Scheduler.S) = struct
  module Reagent = Reagent.Make (Sched)
  module Channel = Channel.Make (Sched)
  module MSQueue = MSQueue.Make (Sched)
end
