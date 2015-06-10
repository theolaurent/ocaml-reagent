(*
 * shift / reset
 *
 * As the type of shift and reset must be linked in some way,
 * three possibilities : additional parameter, monomorphisation and a functor.
 *
 * This piece of code would be really more interesting with a continuation duplication !
 *
 * Besides, it could maybe become really generic with modular implicits..
 *)

module ParamShiftReset : sig
  type 'a t
  val make_witness : unit -> 'a t
  val reset : 'a t -> (unit -> 'a) -> 'a
  val shift : 'a t -> (('b -> 'a) -> 'a) -> 'b
end = struct
  module type t = sig type v effect Shift : (('a -> v) -> v) -> 'a end

  type 'a t = (module t with type v = 'a)

  let make_witness (type u) () : u t =
    let module M = struct
        type v = u
        effect Shift : (('a -> v) -> v) -> 'a
      end in
    (module M)

  let reset (type u) (w:u t) code =
    let module M = (val w) in
    match code () with
    | v -> v
    | effect (M.Shift f) k -> f (continue k)


  let shift (type u) (w:u t) f =
    let module M = (val w) in
    perform (M.Shift f)
end

module MonoShiftReset : sig
  type 'b t = { reset : (unit -> 'b) -> 'b ;
                shift : 'a. (('a -> 'b) -> 'b) -> 'a }
  val create : unit -> 'a t
end = struct
  type 'b t = { reset : (unit -> 'b) -> 'b ;
                shift : 'a. (('a -> 'b) -> 'b) -> 'a }

  let create (type v) () =
    let module M = struct
        effect Shift : (('a -> v) -> v) -> 'a
      end in
    let reset code = match code () with
      | v -> v
      | effect (M.Shift f) k -> f (continue k)
    in
    let shift f = perform (M.Shift f)
    in { reset ; shift }
end


module FunctShiftReset (X: sig type t end) : sig
  val reset : (unit -> X.t) -> X.t
  val shift : (('a -> X.t) -> X.t) -> 'a
end = struct
  effect Shift : (('a -> X.t) -> X.t) -> 'a

  let reset code =
    match code () with
    | v -> v
    | effect (Shift f) k -> f (continue k)


  let shift f =
    perform (Shift f)
end
