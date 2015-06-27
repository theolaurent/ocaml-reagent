
type ('a, 'b) t = {
    tryReact : 'a -> Reaction.t -> 'b Offer.t option -> 'b option ; (* None stands for Block *)
    composeI : 'c . ('b, 'c) t -> ('a, 'c) t ;
  }

(* for the moment just try without and then with offer; cf scala implem (canSync...) *)

let compose r1 r2 = r1.composeI r2

let run r a =
  match r.tryReact a Reaction.inert None with
  (* for now, no retry, cf scala code *)
  | None -> perform (Sched.Suspend (fun k -> ignore (r.tryReact a Reaction.inert (Some (Offer.make k)))))
  | Some x -> x


let commit : ('a, 'a) t =
  let tryReact a rx offer =
    let () = match offer with
      | None -> ()
      | Some o -> if Offer.try_abort o then ()
                  else failwith "Reagent.commit: I have to think about this case..."
    in
    if Reaction.try_commit rx then
      Some a
    else failwith "Reagent.commit: that shouldn't happen yet, no parallelism."
  in
  let composeI (type b) (r:('a, b) t) = r
  in { tryReact ; composeI }

(*
let rec never () =
  let tryReact a rx offer =
    None
  in
(*let composeI (type c) (r:(u, c) t) = never () *)
(*in { tryReact ; composeI } *)
  let composeI = (fun _ -> never ())
  in { tryReact ; composeI = Obj.magic composeI }
*)

let rec choice (r1:('a, 'b) t) (r2:('a, 'b) t) =
  let tryReact a rx offer =
    match r1.tryReact a rx offer with
    | None -> r2.tryReact a rx offer
    | Some x -> Some x
  in
(*let compose (type c) (r:('b, c) t) = *)
(*  (* hmm what is this case Choice thing in the scala code? *) *)
(*  choice (r1.composeI r) (r2.composeI r) *)
(*in { tryReact ; composeI } *)
  let composeI = (fun r -> choice (r1.composeI r) (r2.composeI r))
  in { tryReact ; composeI = Obj.magic composeI }

(* TODO: post commit => what is this auto cont thing? *)

(* moved from offer.ml to avoid circular dependencies *)
let comsume_and_continue o complete_with continue_with rx k enclosing_offer =
  (* forgetting the immediate CAS for now; cf scala implem *)
  let new_rx = Reaction.add_pc (Offer.rx_with_completion o rx complete_with)
                               (Offer.wake o) in
  (* for now, only blocking ones; cf scala implem *)
  k.tryReact continue_with new_rx enclosing_offer
