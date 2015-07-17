
type ('a, 'b) endpoint

val create : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint

val swap : ('a, 'b) endpoint -> ('a, 'b) Reagent.t
