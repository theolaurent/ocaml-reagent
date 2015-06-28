
type 'a ref

val ref : 'a -> 'a ref

val get : 'a ref -> 'a

val docas : 'a ref -> ov:'a -> nv:'a -> bool

type abstract_t

val build : 'a ref -> ov:'a -> nv:'a -> abstract_t

val commit : abstract_t -> bool

val kCAS : abstract_t list -> bool


module Sugar : sig
  type 'a casref_update
  type 'a casref = 'a ref
  val ref : 'a -> 'a casref
  val (!) : 'a casref -> 'a
  val (-->) : 'a -> 'a -> 'a casref_update
  val (<!=) : 'a casref -> 'a casref_update -> bool
  val (<:=) : 'a casref -> 'a casref_update -> abstract_t
end
