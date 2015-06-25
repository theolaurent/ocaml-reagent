module type SCHED = sig
  type 'a cont
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume : 'a cont * 'a -> unit
end

module Make : functor (Sc : SCHED) -> sig

(*module Offer : sig
    type ('a, 'b) t
    exception AlreadyFulfilledOffer
    val make : 'a -> 'b Sc.cont -> ('a, 'b) t
    val set_fulfilled : ('a, 'b) t -> 'a * 'b Sc.cont
    val is_fulfilled : ('a, 'b) t -> bool
    val clean_queue : ('a, 'b) t Queue.t -> unit
  end*)

  module Events :
    sig
      type ('a, 'b) t
      val sync : ('a, 'b) t -> 'a -> 'b
      val choose : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    end

  module Mvar :
    sig
      type 'a mvar
      val new_mvar : 'a -> 'a mvar
      val new_empty_mvar : unit -> 'a mvar
      val put_mvar_evt : 'a mvar -> ('a, unit) Events.t
      val take_mvar_evt : 'a mvar -> (unit, 'a) Events.t
    end

end
