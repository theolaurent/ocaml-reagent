
type 'a result =
  | Final of 'a * Reaction.t
  | WithOffer of ('a Offer.t -> unit)

type ('a, 'b, 'r) t =
  { tryReact : 'c . arg:'a -> rx:Reaction.t -> next:('b, 'c, 'r) t option -> 'r result }

(* TODO : test *)
(* type ('a, 'b, 'r) t = *)
(*   { tryReact : arg:'a -> rx:Reaction.t -> next:('b, 'r) cont -> 'r result } *)
(* and ('a, 'b) cont = *)
(*   | Nope : ('a, 'a) cont *)
(*   | Next : ('a, 'b) t_hidden -> ('a, 'b) cont *)
(* and ('a, 'b) t_hidden = { it : 'c . ('a, 'c, 'b) t } *)

(* TODO: redo that one *) (*
(* moved from offer.ml to avoid circular dependencies *)
let comsume_and_continue o v ctx =
  (* forgetting the immediate CAS for now; cf scala implem *)
  let new_rx = Reaction.add_pc (Offer.rx_with_completion o ctx.rx v)
                               (Offer.wake o) in
  (* for now, only blocking ones; cf scala implem *)
  ctx.next.tryReact { ctx with rx = new_rx }
  *)

(* for the moment just try without and then with offer; cf scala implem (canSync...) *)
let run r arg =
  match r.tryReact ~arg ~rx:Reaction.inert ~next:None with
  (* for now, no retry, cf scala code *)
  | Final (x, rx) -> ( if Reaction.try_commit rx then x
                       else failwith "No transient failure for now" )
  | WithOffer f -> perform (Sched.Suspend (fun k -> f (Offer.make k)))


let rec pipe (r1:('a, 'b, 'r) t) (r2:('b, 'c, 'r) t) : ('a, 'c, 'r) t =
  let tryReact (type u) ~arg ~rx ~(next:('c, u, 'r) t option) =
    r1.tryReact ~arg ~rx ~next:(Some (pipe r2 (Obj.magic next))) (* TODO: solve this type problem *)
  in { tryReact }


let rec choice r1 r2 =
  let tryReact ~arg ~rx ~next = match r1.tryReact ~arg ~rx ~next with
    | WithOffer f -> begin match r2.tryReact ~arg ~rx ~next with
                           | WithOffer g -> WithOffer (fun o -> f o ; g o)
                           | Final (a, rx) -> Final (a, rx)
                     end
    | Final (a, rx) -> Final (a, rx)
  in { tryReact }

let rec never () =
  let tryReact ~arg ~rx ~next =
    WithOffer (fun _ -> ()) (* be careful, will the maybe no-more-referenced Sched.cont be deleted? *)
  in { tryReact }
