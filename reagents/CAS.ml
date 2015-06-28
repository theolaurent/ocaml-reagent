
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
let docas r ~expect ~update =
  (* lock? *)
  match r.content with
  | Normal x when x = expect -> ( r.content <- Normal update ; true )
  | _                    -> false
  (* unlock? *)

(* Isn't that heavy at runtime? OCaml should definetly have existentials. *)
type 'a t = { expect : 'a ; update : 'a ; r : 'a ref }
module type abstract_t = sig type u val it : u t end
type abstract_t = (module abstract_t)

let build (type v) (r:v ref) ~(expect:v) ~(update:v) =
  let module M = struct
      type u = v
      let it = { expect ; update ; r }
    end in (module M : abstract_t)

let commit cas =
  let module M = (val cas : abstract_t) in
  let { r ; expect ; update } = M.it in
  docas r ~expect ~update

let semicas cas =
  let module M = (val cas : abstract_t) in
  let { r ; expect ; _ } = M.it in
  (* lock? *)
  match r.content with
  | Normal x when x = expect -> ( r.content <- OnGoingKCAS x ; true )
  | _                    -> false
  (* unlock? *)

(* only the tread that perform the semicas should be able to rollbwd/fwd *)
let rollbwd cas =
  let module M = (val cas : abstract_t) in
  let { r ; _ } = M.it in
  (* lock? *)
  match r.content with
  | Normal _ -> assert false
  | OnGoingKCAS x -> r.content <- Normal x
  (* unlock? *)

let rollfwd cas =
  let module M = (val cas : abstract_t) in
  let { r ; update ; expect } = M.it in
  (* lock? *)
  match r.content with
  | Normal _ -> assert false
  | OnGoingKCAS x -> ( assert (x = expect) ; r.content <- Normal update )
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
  val (<:=) : 'a casref -> 'a casref_update -> abstract_t
end = struct
  type 'a casref_update = { expect : 'a ;
                            update : 'a }
  type 'a casref = 'a ref
  let ref x = ref x
  let (!) r = get r

  let (-->) expect update = { expect ; update }

  let (<!=) r { expect ; update } = docas r ~expect ~update
  let (<:=) r { expect ; update } = build r ~expect ~update
end
