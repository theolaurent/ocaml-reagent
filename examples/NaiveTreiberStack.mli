
type 'a t

val push : 'a t -> ('a, unit) Reagent.t

val tryPop : 'a t -> (unit, 'a option) Reagent.t
