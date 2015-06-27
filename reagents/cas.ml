
(* TODO: afterCAS actions!! *)

type 'a refstate =
  | Normal of 'a
  | OnGoingKCAS of 'a (* store the old value *)

type 'a ref = { mutable content : 'a refstate }

let ref x = { content = Normal x }

let get r = match r.content with
  | Normal x -> x
  | OnGoingKCAS x -> x

(* waiting for the hardware version *)
let cas r ~ov ~nv =
  (* lock? *)
  match r.content with
  | Normal x when x = ov -> ( r.content <- Normal nv ; true )
  | _                    -> false
  (* unlock? *)


module Sugar : sig
  type 'a casref_update
  val ( ! ) : 'a ref -> 'a
  val ( --> ) : 'a -> 'a -> 'a casref_update
  val ( := ) : 'a ref -> 'a casref_update -> bool
end = struct
  type 'a casref_update = { ov : 'a ;
                            nv : 'a }
  let (!) r = get r

  let (-->) ov nv = { ov ; nv }

  let (:=) r { ov ; nv } = cas r ~ov ~nv
end

module KCAS : sig
  type atom
  val build_atom : 'a ref -> ov:'a -> nv:'a -> atom
  val kCAS : atom list -> bool
end = struct

  (* Isn't that heavy at runtime? OCaml should definetly have existentials. *)
  type 'a atom_struct = { ov : 'a ; nv : 'a ; r : 'a ref }
  module type atom = sig type t val it : t atom_struct end
  type atom = (module atom)

  let build_atom (type u) (r:u ref) ~(ov:u) ~(nv:u) =
    let module M = struct
        type t = u
        let it = { ov ; nv ; r }
      end in (module M : atom)

  let semicas cas =
    let module M = (val cas : atom) in
    let { r ; ov ; _ } = M.it in
    (* lock? *)
    match r.content with
    | Normal x when x = ov -> ( r.content <- OnGoingKCAS x ; true )
    | _                    -> false
    (* unlock? *)

  (* only the tread that perform the semicas should be able to rollbwd/fwd *)
  let rollbwd cas =
    let module M = (val cas : atom) in
    let { r ; _ } = M.it in
    (* lock? *)
    match r.content with
    | Normal _ -> assert false
    | OnGoingKCAS x -> r.content <- Normal x
    (* unlock? *)

  let rollfwd cas =
    let module M = (val cas : atom) in
    let { r ; nv ; ov } = M.it in
    (* lock? *)
    match r.content with
    | Normal _ -> assert false
    | OnGoingKCAS x -> ( assert (x = ov) ; r.content <- Normal nv )
    (* unlock? *)

  let kCAS l =
    if List.for_all (fun cas -> semicas cas) l then
      ( List.iter rollfwd l ; true )
    else
      ( List.iter rollbwd l ; false )

end

include KCAS
