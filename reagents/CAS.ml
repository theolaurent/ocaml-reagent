
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
let docas r ~ov ~nv =
  (* lock? *)
  match r.content with
  | Normal x when x = ov -> ( r.content <- Normal nv ; true )
  | _                    -> false
  (* unlock? *)

(* Isn't that heavy at runtime? OCaml should definetly have existentials. *)
type 'a t_struct = { ov : 'a ; nv : 'a ; r : 'a ref }
module type t = sig type t val it : t t_struct end
type t = (module t)

let build (type u) (r:u ref) ~(ov:u) ~(nv:u) =
  let module M = struct
      type t = u
      let it = { ov ; nv ; r }
    end in (module M : t)

let commit cas =
  let module M = (val cas : t) in
  let { r ; ov ; nv } = M.it in
  docas r ~ov ~nv

let semicas cas =
  let module M = (val cas : t) in
  let { r ; ov ; _ } = M.it in
  (* lock? *)
  match r.content with
  | Normal x when x = ov -> ( r.content <- OnGoingKCAS x ; true )
  | _                    -> false
  (* unlock? *)

(* only the tread that perform the semicas should be able to rollbwd/fwd *)
let rollbwd cas =
  let module M = (val cas : t) in
  let { r ; _ } = M.it in
  (* lock? *)
  match r.content with
  | Normal _ -> assert false
  | OnGoingKCAS x -> r.content <- Normal x
  (* unlock? *)

let rollfwd cas =
  let module M = (val cas : t) in
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

module Sugar : sig
  type 'a casref_update
  type 'a casref = 'a ref
  val ref : 'a -> 'a casref
  val (!) : 'a casref -> 'a
  val (-->) : 'a -> 'a -> 'a casref_update
  val (<!=) : 'a casref -> 'a casref_update -> bool
  val (<:=) : 'a casref -> 'a casref_update -> t
end = struct
  type 'a casref_update = { ov : 'a ;
                            nv : 'a }
  type 'a casref = 'a ref
  let ref x = ref x
  let (!) r = get r

  let (-->) ov nv = { ov ; nv }

  let (<!=) r { ov ; nv } = docas r ~ov ~nv
  let (<:=) r { ov ; nv } = build r ~ov ~nv
end
