
(* TODO: afterCAS actions!! *)

type 'a updt = { expect : 'a ; update : 'a }

type 'a state =
  | Normal of 'a
  | OnGoingKCAS of 'a

type 'a ref = { mutable content : 'a state ;
                        id      : int      }

let compare_and_swap r x y =
  Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y)
                             (* 0 stands for the first field *)

let ref x = { content = Normal x           ;
              id      = Oo.id (object end) }
            (* I've been told this is supposed *)
            (* to be a thread-safe unique id   *)

type refid = int

let id r = r.id

let get r = match r.content with
  | Normal a -> a
  | OnGoingKCAS a -> a

type t =
  | CAS : 'a ref * 'a updt -> t


let commit (CAS (r, { expect ; update })) =
  let s = r.content in
  match s with
  | Normal a when a == expect -> compare_and_swap r s (Normal update)
  | _                         -> false

let semicas (CAS (r, { expect ; _ })) =
  let s = r.content in
  match s with
  | Normal a when a == expect -> compare_and_swap r s (OnGoingKCAS a)
  | _                         -> false

(* only the tread that perform the semicas should be able to rollbwd/fwd *)
(* so we don't need to cas, just set the field to the new value          *)
let rollbwd (CAS (r, _)) =
  match r.content with
  | Normal _      -> failwith "CAS.kCAS: broken invariant"
  | OnGoingKCAS x -> r.content <- Normal x

let rollfwd (CAS (r, { update ; _ })) =
  match r.content with
  | Normal _ -> failwith "CAS.kCAS: broken invariant"
  | OnGoingKCAS x ->  r.content <- Normal update
                    (* we know we have x == expect *)

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

open Sugar

let try_map f r =
  let s = !r in (r <!= s --> f s)

let map f r =
  let b = Backoff.create () in
  let rec loop () =
    if try_map f r then ()
    else ( Backoff.once b ; loop ())
  in loop ()

let incr r = map (fun x -> x + 1) r
let decr r = map (fun x -> x - 1) r
