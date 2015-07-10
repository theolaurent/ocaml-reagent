type 'a cont

val suspend : ('a cont -> unit) -> 'a

val resume : 'a cont -> 'a -> unit

val fork : (unit -> unit) -> unit

val yield : unit -> unit

val get_tid : unit -> int



val run : (unit -> unit) -> unit
