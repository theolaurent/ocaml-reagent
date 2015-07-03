
type ('a, 'b) t

val run : ('a, 'b) t -> 'a -> 'b

val pipe : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

val choose : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val constant : 'a -> ('b, 'a) t

val never : ('a, 'b) t

val noop : ('a, 'a) t

val lift : ('a -> 'b) -> ('a, 'b) t

val computed : ('a -> (unit, 'b) t) -> ('a, 'b) t

val cas : 'a CAS.Sugar.casref -> 'a CAS.Sugar.casupdt -> (unit, unit) t

val post_commit : ('a -> unit) -> ('a, 'a) t

val pair : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

type ('a, 'b) message

val is_message_available : ('a, 'b) message -> bool

val send : (('a, 'b) message -> unit) -> ('a, 'b) t

val answer : ('a, 'b) message -> ('b, 'a) t

module Sugar :
  sig
    val ( * ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
    val ( || ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val ( |> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  end
