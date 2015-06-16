
type 'a t

val create : ('a -> 'b) -> 'a -> 'b t

val get_thread : 'a t -> Thread.t

val get_result : 'a t -> 'a
