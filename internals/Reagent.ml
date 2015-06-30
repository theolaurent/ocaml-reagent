
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim block / non block                         *)
(* TODO: optim when only one cas ; but seems to need CPS *)

type ('a, 'b) t = {
    buildReact : 'a -> 'b Offer.t -> unit
  }


let run r arg =
  let { rx ; result } = Offer.suspend (r.buildReact arg) in
  if Reaction.try_commit rx then result
  else failwith "Reagent.run: No transient failure for now"


(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let pipe (r1:('a, 'b) t) (r2:('b, 'c) t) : ('a, 'c) t =
  let buildReact arg k =
    let { rx = rx1 ; result = res1 } = Offer.suspend (r1.buildReact arg) in
    let { rx = rx2 ; result = res2 } = Offer.suspend (r2.buildReact res1) in
    ignore (Offer.try_resume k { rx = Reaction.combine rx1 rx2 ; result = res2})
  in { buildReact }

(* TODO: I think the build should be concurrent. Also cf pair.              *)
let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let buildReact arg k =
    r1.buildReact arg k ; r2.buildReact arg k
  in { buildReact }

let constant (x:'a) : ('b, 'a) t =
  let buildReact _ k =
    ignore (Offer.try_resume k (return x))
  in { buildReact }

let never : ('a, 'b) t =
  let buildReact _ k = () (* TODO: will the GC collect the maybe *)
                           (* unreferenced continuation?         *)
  in { buildReact }

let noop : ('a, 'a) t =
  let buildReact a k =
    ignore (Offer.try_resume k (return a))
  in { buildReact }


(* read ref should not be a reagent. Indeend it can *)
(* be confusing when piping as read ref is truly    *)
(* equivalent to const (!r)                         *)
(*                                                  *)
(* let read (r:'a casref) : (unit, 'a) t =          *)
(*  let buildReact () k =                           *)
(*    Offer.resume k (return !r)       *)
(*  in { buildReact }                               *)
(*                                                  *)

let cas (r:'a casref) (updt:'a casupdt) : (unit, unit) t =
  let buildReact () k =
    let rx = Reaction.cas r updt
    in ignore (Offer.try_resume k { rx ; result = () })
  in { buildReact }
(* TODO: Hmm but the actual cas will not be performed until the commit phase ..   *)
(* Thus, to cas piped are bound to fail? (the problem being cas >> read is false! *)
(* I think this version behave as the scala code thought  ..                      *)


(* f is total for the moment (contrary to the scala version of lift) *)
(* TODO: when trasient failure, partial, with option and / or exn    *)
(*                                                                   *)
(* Be careful with lift and computed, their behaviour can be non-    *)
(* intuistic when sequenced. Remember that a reaction is two-phased. *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let buildReact a k =
    ignore (Offer.try_resume k (return (f a)))
  in { buildReact }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let buildReact arg k =
    (f arg).buildReact () k
  in { buildReact }


(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction should be concurrent... (not as in the scala version!)   *)
(* Also cf choose.                                                       *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let buildReact (a, b) k =
    let { rx ; result } = Offer.suspend (r.buildReact a) in
    ignore (Offer.try_resume k { rx ; result = (result, b) })
  in { buildReact }
let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let buildReact (a, b) k =
    let { rx ; result } = Offer.suspend (r.buildReact b) in
    ignore (Offer.try_resume k { rx ; result = (a, result) })
  in { buildReact }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  pipe (lift (fun a -> (a, a)))
       (pipe (first r1) (second r2))

module Sugar = struct
  let ( * ) = pair
  let ( + ) = choose
  let ( >> ) = pipe
end
