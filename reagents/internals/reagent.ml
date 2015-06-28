
type 'a result =
  | Imm of 'a
  | WithOffer of ('a Offer.t -> unit)


(* recursive module trick to get a mutualy recursive type definition with *)
(* an first class module ; all that mess for an existential type [...]    *)
module rec M : sig
  module type t_cont = sig type u type v type z val it : (u, z, v) M.t end

  type ('a, 'b, 'c) t = { tryReact : arg:'a -> rx:Reaction.t -> next: ('b, 'c) t_cont -> 'c result }
  and ('a, 'b) t_cont = (module t_cont with type u = 'a and type v = 'b)
end = struct
  module type t_cont = sig type u type v type z val it : (u, z, v) M.t end

  type ('a, 'b, 'c) t = { tryReact : arg:'a -> rx:Reaction.t -> next: ('b, 'c) t_cont -> 'c result }
  and ('a, 'b) t_cont = (module t_cont with type u = 'a and type v = 'b)
end

include M

let t_embed (type u1) (type v1) (type z1) (r:(u1, z1, v1) t) =
  let module M = struct
      type u = u1
      type v = v1
      type z = z1
      let it = r
    end in ((module M) : (u1, v1) t_cont)


(* TODO: redo that one *) (*
(* moved from offer.ml to avoid circular dependencies *)
let comsume_and_continue o v ctx =
  (* forgetting the immediate CAS for now; cf scala implem *)
  let new_rx = Reaction.add_pc (Offer.rx_with_completion o ctx.rx v)
                               (Offer.wake o) in
  (* for now, only blocking ones; cf scala implem *)
  ctx.next.tryReact { ctx with rx = new_rx }
  *)

let rec commit : ('a, 'a, 'a) t =
  let tryReact (type u) ~arg ~rx ~(next:(u, u) t_cont) =
    let module M = (val next) in
    let next = M.it in
    let () = assert (Obj.magic next = commit) ; (* there are way to enforce that at type-level ! *)
             ( if Reaction.try_commit rx then ()
               else failwith "No transient failure for now" )
    in Imm arg
  in { tryReact }

(* for the moment just try without and then with offer; cf scala implem (canSync...) *)
let run r arg =
  match r.tryReact ~arg ~rx:Reaction.inert ~next:(t_embed commit) with
  (* for now, no retry, cf scala code *)
  | Imm x -> x
  | WithOffer f -> perform (Sched.Suspend (fun k -> f (Offer.make k)))


let pipe r1 r2 =
  let rec aux (type u) (type v) r (next:(u, v) t_cont) =
    let module M = (val next) in
    let thenext = M.it in
    (* let tryReact ~arg ~rx ~next = r.tryReact ~arg ~rx ~next:(aux thenext next) in *)
    let tryReact ~arg ~rx ~next = r.tryReact ~arg ~rx ~next:(aux (Obj.magic thenext) next) in
    t_embed { tryReact }
  in aux r1 (t_embed r2)


let rec choice r1 r2 =
  let tryReact ~arg ~rx ~next = match r1.tryReact arg rx next with
    | WithOffer f -> begin match r2.tryReact arg rx next with
                           | WithOffer g -> WithOffer (fun o -> f o ; g o)
                           | Imm a -> Imm a
                     end
    | Imm a -> Imm a
  in { tryReact }

let rec never () =
  let tryReact ~arg ~rx ~next =
    WithOffer (fun _ -> ()) (* be careful, will the maybe no-more-referenced Sched.cont be deleted? *)
  in { tryReact }
