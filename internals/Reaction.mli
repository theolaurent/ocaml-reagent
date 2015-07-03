type 'a t

val cas : CAS.t -> unit t

val pc : (unit -> unit) -> unit t

val try_commit : 'a t -> bool

val get_value : 'a t -> 'a

val return : 'a -> 'a t

val clear : 'a t -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

module Sugar :
  sig
    val rx_return : 'a -> 'a t
    val rx_value : 'a t -> 'a
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t
    val ( !! ) : 'a t -> bool
  end
