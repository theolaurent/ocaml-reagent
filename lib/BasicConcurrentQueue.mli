
type 'a t

exception Empty

val create : unit -> 'a t

val push : 'a -> 'a t -> unit

val try_pop : 'a t -> 'a option

val is_empty : 'a t -> bool
