(*
 * shift / reset
 *
 * As the type of shift and reset must be linked in some way,
 * two possibilities : additional parameter and monomorphisation.
 *
 * This piece of code would be really more interesting with a continuation duplication !
 *
 * Besides, it could become really generic with modular implicits ?
 *)

module ParamShiftReset : sig
  type 'a wit
  val make_wit : unit -> 'a wit
  val reset : 'a wit -> (unit -> 'a) -> 'a
  val shift : 'a wit -> (('b -> 'a) -> 'a) -> 'b
end = struct
  module type wit = sig type v effect Shift : (('a -> v) -> v) -> 'a end

  type 'a wit = (module wit with type v = 'a)

  let make_wit (type u) () : u wit =
    let module M = struct
        type v = u
        effect Shift : (('a -> v) -> v) -> 'a
      end in
    (module M)

  let reset (type u) (w:u wit) code =
    let module M = (val w) in
    match code () with
    | v -> v
    | effect (M.Shift f) k -> f (continue k)


  let shift (type u) (w:u wit) f =
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

(* The same, without the overhead of first-class modules. *)
(*
 * Be carefull, these are not exactly the same functions.
 * Two different instances of the code above are never compatible while
 * below, two different instances that have the same type are perfeclty
 * interchangeable
 *)

module ParamShiftReset_ : sig
  type 'a wit
  val make_wit : unit -> 'a wit
  val reset : 'a wit -> (unit -> 'a) -> 'a
  val shift : 'a wit -> (('b -> 'a) -> 'a) -> 'b
end = struct

  effect Shift : (('a -> 'b) -> 'b) -> 'a

  type 'a wit

  let make_wit () = Obj.magic ()

  let reset w code =
    match code () with
    | v -> v
    | effect (Shift f) k -> (Obj.magic f) (continue k)


  let shift w f =
    perform (Shift f)
end

module MonoShiftReset_ : sig
  type 'b t = { reset : (unit -> 'b) -> 'b ;
                shift : 'a. (('a -> 'b) -> 'b) -> 'a }
  val create : unit -> 'a t
end = struct
  type 'b t = { reset : (unit -> 'b) -> 'b ;
                shift : 'a. (('a -> 'b) -> 'b) -> 'a }

  effect Shift : (('a -> 'b) -> 'b) -> 'a

  let create () =
    let reset code = match code () with
      | v -> v
      | effect (Shift f) k -> (Obj.magic f) (continue k)
    in
    let shift f = perform (Shift f)
    in { reset ; shift }
end
