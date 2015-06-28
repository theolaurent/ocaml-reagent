
(* TODO : optim block / non block *)

(* TODO : cas offers *)
type ('a, 'b) t = {
    buildReact : 'a -> (Reaction.t * 'b) Conthread.t -> unit
  }

let run r arg =
  let (rx, res) = Conthread.suspend (r.buildReact arg) in
  if Reaction.try_commit rx then res
  else failwith "Reagent.run: No transient failure for now"


let pipe (r1:('a, 'b) t) (r2:('b, 'c) t) : ('a, 'c) t =
  let buildReact arg k =
    let (rx1, res1) = Conthread.suspend (r1.buildReact arg) in
    let (rx2, res2) = Conthread.suspend (r2.buildReact res1) in
    Conthread.try_fulfill_and_wake_ignore
      k (Reaction.combine rx1 rx2, res2)
  in { buildReact }

let choice (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let buildReact arg k =
    r1.buildReact arg k ; r2.buildReact arg k
  in { buildReact }

let constant (x:'b) : (unit, 'b) t =
  let buildReact () k =
    Conthread.try_fulfill_and_wake_ignore
      k (Reaction.inert, x)
  in { buildReact }

let never : (unit, 'a) t =
  let buildReact () k = () (* TODO: will the GC collect the maybe *)
                           (* unreferenced continuation?         *)
  in { buildReact }
