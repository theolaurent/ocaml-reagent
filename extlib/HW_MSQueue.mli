

type 'a t

val create : unit -> 'a t

val push : 'a -> 'a t -> unit

val pop : 'a t -> 'a option
