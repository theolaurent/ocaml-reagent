
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
  type 'a atom = { ov : 'a; nv : 'a; r : 'a ref; }
  val kCAS : 'a atom list -> bool
end = struct

  type 'a atom = { ov : 'a ; nv : 'a ; r : 'a ref }

  let semicas { r ; ov ; _ } =
    (* lock? *)
    match r.content with
    | Normal x when x = ov -> ( r.content <- OnGoingKCAS x ; true )
    | _                    -> false
    (* unlock? *)

  (* only the tread that perform the semicas should be able to rollbwd/fwd *)
  let rollbwd { r ; _ } =
    (* lock? *)
    match r.content with
    | Normal _ -> assert false
    | OnGoingKCAS x -> r.content <- Normal x
    (* unlock? *)

  let rollfwd { r ; nv ; ov } =
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
