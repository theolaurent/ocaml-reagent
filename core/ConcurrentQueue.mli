
type 'a t

val create : unit -> 'a t

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val push : 'a t -> ('a, unit) Reagent.t

val pop : 'a t -> (unit, 'a) Reagent.t

val pop_until : 'a t -> ('a -> bool) -> (unit, unit) Reagent.t

type 'a cursor

val snapshot : 'a t -> 'a cursor

val next : 'a cursor -> ('a * 'a cursor) option
