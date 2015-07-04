
type 'a t

type id

val id : 'a t -> id

val is_waiting : 'a t -> bool

val wake : 'a t -> unit -> unit

val complete_cas : 'a t -> 'a -> CAS.t

val suspend : ('a t -> unit) -> 'a

val try_resume : 'a t -> 'a -> bool
