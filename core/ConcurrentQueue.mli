
type 'a t

val create : unit -> 'a t

val whole : 'a t -> 'a FQueue.t

val push : 'a t -> ('a, unit) Reagent.t

val pop : 'a t -> (unit, 'a) Reagent.t

val pop_until : 'a t -> ('a -> bool) -> (unit, unit) Reagent.t
