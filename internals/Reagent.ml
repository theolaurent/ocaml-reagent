
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim block / non block                         *)
(* TODO: optim when only one cas                         *)

type ('a, 'b) t = { tryReact : 'r . 'a -> Reaction.t
                                       -> ('b, 'r) t
                                       -> 'r Offer.t -> unit ;
                    isCommit : bool
                  }

(* TODO: find a way to explain what's going on to the type system and *)
(*       get rid of Obj.magic                                         *)
let rec commit : 'a . ('a, 'a) t =
  let tryReact arg rx next offer =
    if next == Obj.magic commit then
      ( if !! rx
        then ignore (Offer.try_resume offer (Obj.magic arg))
        else failwith "Reagent.commit: No transient failure for now" )
    else
      next.tryReact arg rx commit offer
  in { tryReact ; isCommit = true }

let run : ('a, 'b) t -> 'a -> 'b =
  (fun r arg -> Offer.suspend (r.tryReact arg Reaction.inert commit))

(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let rec pipe : 'a 'b 'c . ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t = (fun r1 r2 ->
   let tryReact arg rx next offer =
     r1.tryReact arg rx
                 ( if next.isCommit then Obj.magic r2
                   else pipe r2 next ) offer
   in { tryReact ; isCommit = false }
  )

(* TODO: I think the build should be concurrent.                            *)
(*       Humm wait it depend on what you want cf tempt reagent.             *)
let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let tryReact arg rx next offer =
    r1.tryReact arg rx next offer ; r2.tryReact arg rx next offer
  in { tryReact ; isCommit = false }

let constant (x:'a) : ('b, 'a) t =
  let tryReact _ rx next offer =
    next.tryReact x rx commit offer
  in { tryReact ; isCommit = false }

let never : ('a, 'b) t =
  let tryReact _ _ _ _ = ()
  in { tryReact ; isCommit = false }

let noop : ('a, 'a) t =
  let tryReact arg rx next offer =
    next.tryReact arg rx commit offer
  in { tryReact ; isCommit = false }


(* read ref should not be a reagent. Indeend it can *)
(* be confusing when piping as read ref is truly    *)
(* equivalent to const (!r)                         *)

let cas (r:'a casref) (updt:'a casupdt) : (unit, unit) t =
  let tryReact () rx next offer =
    next.tryReact () (rx ++ Reaction.cas r updt) commit offer
  in { tryReact ; isCommit = false }
(* TODO: Hmm but the actual cas will not be performed until the commit phase ..    *)
(* Thus, two cas piped are bound to fail? (the problem being cas >> read is false! *)
(* I think this version behave as the scala code thought  ..                       *)


(* f is total for the moment (contrary to the scala version of lift) *)
(* TODO: when trasient failure, partial, with option and / or exn    *)
(*                                                                   *)
(* Be careful with lift and computed, their behaviour can be non-    *)
(* intuistic when sequenced. Remember that a reaction is two-phased. *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let tryReact arg rx next offer =
    next.tryReact (f arg) rx commit offer
  in { tryReact ; isCommit = false }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let tryReact arg rx next offer =
    (f arg).tryReact () rx next offer
  in { tryReact ; isCommit = false }


(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction might be concurrent... (not as in the scala version!)    *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let tryReact (a, c) rx next offer =
    (pipe r (lift (fun b -> (b, c)))).tryReact a rx next offer
  in { tryReact ; isCommit = false }

let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let tryReact (c, a) rx next offer =
    (pipe r (lift (fun b -> (c, b)))).tryReact a rx next offer
  in { tryReact ; isCommit = false }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  pipe (lift (fun a -> (a, a)))
       (pipe (first r1) (second r2))

module Sugar = struct
  let ( * ) = pair
  let ( + ) = choose
  let ( >> ) = pipe
end
