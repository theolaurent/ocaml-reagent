
type 'a ref

val ref : 'a -> 'a ref

val get : 'a ref -> 'a

val cas : 'a ref -> ov:'a -> nv:'a -> bool

module Sugar :
  sig
    type 'a casref_update
    val ( ! ) : 'a ref -> 'a
    val ( --> ) : 'a -> 'a -> 'a casref_update
    val ( := ) : 'a ref -> 'a casref_update -> bool
  end

module KCAS :
  sig
    type 'a atom = { ov : 'a; nv : 'a; r : 'a ref; }
    val kCAS : 'a atom list -> bool
  end
