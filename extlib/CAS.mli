
type 'a ref

type 'a updt = { expect : 'a ; update : 'a }

type refid

val ref : 'a -> 'a ref

val id : 'a ref -> refid

val get : 'a ref -> 'a

type t =
  | CAS : 'a ref * 'a updt -> t

val commit : t -> bool

val kCAS : t list -> bool

val try_map : ('a -> 'a) -> 'a ref -> bool

val map : ('a -> 'a) -> 'a ref -> unit

val incr : int ref -> unit

val decr : int ref -> unit

module Sugar : sig
  type 'a casupdt = 'a updt
  type 'a casref = 'a ref
  val ref : 'a -> 'a casref
  val (!) : 'a casref -> 'a
  val (-->) : 'a -> 'a -> 'a casupdt
  val (<!=) : 'a casref -> 'a casupdt -> bool
  val (<:=) : 'a casref -> 'a casupdt -> t
end