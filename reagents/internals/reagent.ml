
(* TODO: optim block / non block                         *)
(* TODO: optim when only onw cas ; but seems to need CPS *)

type ('a, 'b) t = {
    buildReact : 'a -> (Reaction.t * 'b) Conthread.t -> unit
  }


let run r arg =
  let (rx, res) = Conthread.suspend (r.buildReact arg) in
  if Reaction.try_commit rx then res
  else failwith "Reagent.run: No transient failure for now"


(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let pipe (r1:('a, 'b) t) (r2:('b, 'c) t) : ('a, 'c) t =
  let buildReact arg k =
    let (rx1, res1) = Conthread.suspend (r1.buildReact arg) in
    let (rx2, res2) = Conthread.suspend (r2.buildReact res1) in
    Conthread.resume k (Reaction.combine rx1 rx2, res2)
  in { buildReact }

let (>>) = pipe

let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let buildReact arg k =
    r1.buildReact arg k ; r2.buildReact arg k
  in { buildReact }

let (+) = choose

let constant (x:'a) : ('b, 'a) t =
  let buildReact _ k =
    Conthread.resume k (Reaction.inert, x)
  in { buildReact }

let never : ('a, 'b) t =
  let buildReact _ k = () (* TODO: will the GC collect the maybe *)
                           (* unreferenced continuation?         *)
  in { buildReact }

let noop : ('a, 'a) t =
  let buildReact a k =
    Conthread.resume k (Reaction.inert, a)
  in { buildReact }

(* f is total for the moment (contrary to the scala version of lift) *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let buildReact a k =
    Conthread.resume k (Reaction.inert, f a)
  in { buildReact }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let buildReact arg k =
    (f arg).buildReact () k
  in { buildReact }


(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction should be concurrent... (not as in the scala version!)   *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let buildReact (a, b) k =
    let (rx, res) = Conthread.suspend (r.buildReact a) in
    Conthread.resume k (rx, (res, b))
  in { buildReact }
let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let buildReact (a, b) k =
    let (rx, res) = Conthread.suspend (r.buildReact b) in
    Conthread.resume k (rx, (b, res))
  in { buildReact }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  (lift (fun a -> (a, a))) >> first r1 >> second r2

let ( * ) = pair
