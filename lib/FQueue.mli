
type 'a t

exception Empty

val empty : 'a t

val push : 'a -> 'a t -> 'a t

val pop : 'a t -> 'a * 'a t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a -> 'b) -> 'a t -> 'b t
