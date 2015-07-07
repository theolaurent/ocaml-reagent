
type 'a t

val is_waiting : 'a t -> bool

val wake : 'a t -> unit -> unit

val complete_cas : 'a t -> 'a -> CAS.t

val refid : 'a t -> CAS.refid

val suspend : ('a t -> unit) -> 'a

val try_resume : 'a t -> 'a -> bool
