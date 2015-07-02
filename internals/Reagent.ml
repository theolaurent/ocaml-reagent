
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim block / non block                         *)
(* TODO: optim when only one cas                         *)

type ('a, 'b) t_struct = { withReact : 'r . 'a -> Reaction.t
                                               -> ('b, 'r) t
                                               -> 'r Offer.t -> unit ;
                         }
(* This GADT introduce some heavy type annotations but *)
(* is necessary for the type system to accept what's   *)
(* going on when commiting.                            *)
and (_, _) t =
  | Nope : ('a, 'a) t
  | Reagent : ('a, 'b) t_struct -> ('a, 'b) t


let rec commit : type a b . (a, b) t -> (a, b) t_struct = fun r -> match r with
  | Nope -> let withReact (type c) (type r) (arg:c) rx
                          (next:(c, r) t) (offer:r Offer.t) = match next with
              | Nope -> if !! rx
                        then ignore (Offer.try_resume offer arg)
                        else failwith "Reagent.commit: \
                                       No transient failure for now"
              | _    -> (commit next).withReact arg rx Nope offer
            in { withReact }
  | Reagent r -> r



let run : ('a, 'b) t -> 'a -> 'b =
  (fun r arg -> Offer.suspend ((commit r).withReact arg Reaction.inert Nope))

(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let rec pipe : type a b c . (a, b) t -> (b, c) t -> (a, c) t =
  fun r1 r2 -> match (r1, r2) with
    | (Nope, Nope) -> Nope
    | _ -> let withReact arg rx next offer =
             (commit r1).withReact arg rx (pipe r2 next) offer
           in Reagent { withReact }

(* TODO: I think the build should be concurrent.                            *)
(*       Humm wait it depend on what you want cf tempt reagent.             *)
let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let withReact arg rx next offer =
    (commit r1).withReact arg rx next offer ;
    (commit r2).withReact arg rx next offer ;
  in Reagent { withReact }

let constant (x:'a) : ('b, 'a) t =
  let withReact _ rx next offer =
    (commit next).withReact x rx Nope offer
  in Reagent { withReact }

let never : ('a, 'b) t =
  let withReact _ _ _ _ = ()
  in Reagent { withReact }

let noop : ('a, 'a) t =
  let withReact arg rx next offer =
    (commit next).withReact arg rx Nope offer
  in Reagent { withReact }


(* read ref should not be a reagent. Indeend it can *)
(* be confusing when piping as read ref is truly    *)
(* equivalent to const (!r)                         *)

let cas (r:'a casref) (updt:'a casupdt) : (unit, unit) t =
  let withReact () rx next offer =
    (commit next).withReact () (rx ++ Reaction.cas (r <:= updt)) Nope offer
  in Reagent { withReact }
(* Hmm but the actual cas will not be performed until the commit phase..    *)
(* Yep! But this is not what reagents are for: composing actions atomically *)


(* f is total for the moment (contrary to the scala version of lift) *)
(* TODO: when trasient failure, partial, with option and / or exn    *)
(*                                                                   *)
(* Be careful with lift and computed, their behaviour can be non-    *)
(* intuistic when sequenced. Remember that a reaction is two-phased. *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let withReact arg rx next offer =
    (commit next).withReact (f arg) rx Nope offer
  in Reagent { withReact }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let withReact arg rx next offer =
    (commit (f arg)).withReact () rx next offer
  in Reagent { withReact }


(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction might be concurrent... (not as in the scala version!)    *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let withReact (a, c) rx next offer =
    (commit (pipe r (lift (fun b -> (b, c))))).withReact a rx next offer
  in Reagent { withReact }

let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let withReact (c, a) rx next offer =
    (commit (pipe r (lift (fun b -> (c, b))))).withReact a rx next offer
  in Reagent { withReact }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  pipe (lift (fun a -> (a, a)))
       (pipe (first r1) (second r2))

module Sugar = struct
  let ( * ) = pair
  let ( + ) = choose
  let ( >> ) = pipe
end
