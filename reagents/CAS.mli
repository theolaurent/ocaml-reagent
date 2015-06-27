
type 'a ref

val ref : 'a -> 'a ref

val get : 'a ref -> 'a

val docas : 'a ref -> ov:'a -> nv:'a -> bool

type t

val build : 'a ref -> ov:'a -> nv:'a -> t

val commit : t -> bool

val kCAS : t list -> bool


module Sugar : sig
  type 'a casref_update
  val (!) : 'a ref -> 'a
  val (-->) : 'a -> 'a -> 'a casref_update
  val (<!=) : 'a ref -> 'a casref_update -> bool
  val (<:=) : 'a ref -> 'a casref_update -> t
end
