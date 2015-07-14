
type 'a t

val catalist : unit t

val is_waiting : 'a t -> bool

val rx_complete : 'a t -> 'a -> unit Reaction.t

val rx_has : 'a Reaction.t -> 'b t -> bool

(* TODO: document, internal, only on non blocking, *)
(* return None only when waiting and Some when     *)
(* completing successfully.                        *)
val try_get_result : 'a t -> 'a option

val post_and_suspend : ('a t -> unit) -> 'a

val post_and_return : ('a t -> unit) -> 'a t

(*
val try_resume : 'a t -> 'a -> bool
 *)
