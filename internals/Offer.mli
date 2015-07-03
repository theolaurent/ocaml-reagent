
type 'a t

val is_waiting : 'a t -> bool

val wake : 'a t -> unit -> unit

val complete_cas : 'a t -> 'a Reaction.t -> CAS.t

val suspend : ('a t -> unit) -> 'a Reaction.t

val try_resume : 'a t -> 'a Reaction.t -> bool

val rx_resume : 'a t -> 'a Reaction.t -> unit Reaction.t
