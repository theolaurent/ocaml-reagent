
type t

val inert : t

val add_cas : t -> 'a CAS.Sugar.casref -> expect:'a -> update:'a -> t

val add_pc : t -> (unit -> unit) -> t

val ( ++ ) : t -> t -> t

val try_commit : t -> bool
