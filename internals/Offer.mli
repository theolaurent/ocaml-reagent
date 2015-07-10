
type 'a t

val catalist : unit t

val is_waiting : 'a t -> bool

val rx_complete : 'a t -> 'a -> unit Reaction.t

val rx_has : 'a Reaction.t -> 'b t -> bool

val suspend : ('a t -> unit) -> 'a

(*
val try_resume : 'a t -> 'a -> bool
 *)
