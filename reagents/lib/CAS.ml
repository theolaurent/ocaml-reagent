
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

(* waiting for the hardware version *)
let docas r { expect ; update } =
  (* lock? *)
  match r.content with
  (* | Normal x when x = expect -> ( r.content <- Normal update ; true ) *)
  (* structural equality cause segfault (with continuations?)            *)
  (* Anyway physical equality is not absurd for CAS, I guess that what   *)
  (* hardware does.                                                      *)
  | Normal x when x == expect -> ( r.content <- Normal update ; true )
  | _                         -> false
  (* unlock? *)

(* Isn't that heavy at runtime? OCaml should definetly have existentials. *)
module type t = sig type u val updt : u updt val r : u ref end
type t = (module t)

let build (type v) (r:v ref) (updt:v updt) =
  let module M = struct
      type u = v
      let updt = updt
      let r = r
    end in (module M : t)

let commit cas =
  let module M = (val cas : t) in
  docas M.r M.updt

let semicas cas =
  let module M = (val cas : t) in
  (* lock? *)
  match M.r.content with
  (* again, physical equality *)
  | Normal x when x == M.updt.expect -> ( M.r.content <- OnGoingKCAS x ; true )
  | _                                -> false
  (* unlock? *)

(* only the tread that perform the semicas should be able to rollbwd/fwd *)
let rollbwd cas =
  let module M = (val cas : t) in
  (* lock? *)
  match M.r.content with
  | Normal _      -> assert false
  | OnGoingKCAS x -> M.r.content <- Normal x
  (* unlock? *)

let rollfwd cas =
  let module M = (val cas : t) in
  (* lock? *)
  match M.r.content with
  | Normal _ -> assert false
                     (* still physical eq *)
  | OnGoingKCAS x -> ( assert (x == M.updt.expect) ;
                       M.r.content <- Normal M.updt.update )
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

  let (<!=) = docas
  let (<:=) = build
end
