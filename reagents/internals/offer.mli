
type 'a status

type 'a t

val make : 'a Sched.cont -> 'a t

val wake : 'a t -> unit -> unit

val try_complete : 'a t -> 'a -> bool

val rx_with_completion : 'a t -> Reaction.t -> 'a -> Reaction.t
