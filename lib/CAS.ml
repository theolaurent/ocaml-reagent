
(* TODO: afterCAS actions!! *)

type 'a updt = { expect : 'a ; update : 'a }

type 'a refstate =
  | Normal of 'a
  | OnGoingKCAS of 'a (* store the old value *)

type 'a ref = { mutable content : 'a refstate }

let ref x = { content = Normal x }

let get r = match r.content with
  | Normal x -> x
  | OnGoingKCAS x -> x


type t =
  | CAS : 'a ref * 'a updt -> t

(* waiting for the hardware version *)
let commit (CAS (r, { expect ; update })) =
  (* lock? *)
  match r.content with
  (* | Normal x when x = expect -> ( r.content <- Normal update ; true ) *)
  (* structural equality cause segfault (with continuations?)            *)
  (* Anyway physical equality is not absurd for CAS, I guess that what   *)
  (* hardware does.                                                      *)
  | Normal x when x == expect -> ( r.content <- Normal update ; true )
  | _                         -> false
  (* unlock? *)

let semicas (CAS (r, updt)) =
  (* lock? *)
  match r.content with
  (* again, physical equality *)
  | Normal x when x == updt.expect -> ( r.content <- OnGoingKCAS x ; true )
  | _                                -> false
  (* unlock? *)

(* only the tread that perform the semicas should be able to rollbwd/fwd *)
let rollbwd (CAS (r, updt)) =
  (* lock? *)
  match r.content with
  | Normal _      -> assert false
  | OnGoingKCAS x -> r.content <- Normal x
  (* unlock? *)

let rollfwd (CAS (r, updt)) =
  (* lock? *)
  match r.content with
  | Normal _ -> assert false
                     (* still physical eq *)
  | OnGoingKCAS x -> ( assert (x == updt.expect) ;
                       r.content <- Normal updt.update )
  (* unlock? *)

let kCAS l =
  if List.for_all (fun cas -> semicas cas) l then
    ( List.iter rollfwd l ; true )
  else
    ( List.iter rollbwd l ; false )

module Sugar : sig
  type 'a casupdt = 'a updt
  type 'a casref = 'a ref
  val ref : 'a -> 'a casref
  val (!) : 'a casref -> 'a
  val (-->) : 'a -> 'a -> 'a casupdt
  val (<!=) : 'a casref -> 'a casupdt -> bool
  val (<:=) : 'a casref -> 'a casupdt -> t
end = struct
  type 'a casupdt = 'a updt
  type 'a casref = 'a ref
  let ref x = ref x
  let (!) r = get r

  let (-->) expect update = { expect ; update }

  let (<!=) r u = commit (CAS (r, u))
  let (<:=) r u = CAS (r, u)
end
