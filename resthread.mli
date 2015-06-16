
type 'a t

val create : ('a -> 'b) -> 'a -> 'b t

val thread_id : 'a t -> int

val get_result : 'a t -> 'a
