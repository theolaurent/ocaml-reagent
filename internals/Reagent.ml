
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim block / non block                       *)
(* TODO: optim when only one cas                       *)

type ('a, 'b) t_struct = {
    withReact : 'r . 'a Reaction.t -> ('b, 'r) t -> 'r Offer.t -> unit
  }
(* This GADT introduce some heavy type annotations but *)
(* is necessary for the type system to accept what's   *)
(* going on when commiting.                            *)
and (_, _) t =
  | Nope : ('a, 'a) t
  | Reagent : ('a, 'b) t_struct -> ('a, 'b) t


let rec commit : type a b . (a, b) t -> (a, b) t_struct = fun r -> match r with
  | Nope -> let withReact (type c) (type r) (rx:c Reaction.t)
                          (next:(c, r) t) (offer:r Offer.t) = match next with
              | Nope -> ignore (Offer.try_resume offer rx)
              | _    -> (commit next).withReact rx Nope offer
            in { withReact }
  | Reagent r -> r



let run (r:('a, 'b) t) (arg:'a) : 'b =
  let rx = Offer.suspend ((commit r).withReact (return arg) Nope)
  in if !! rx then Reaction.get_value rx
     else failwith "Reagent.run: No transient failure for now"


(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let rec pipe : type a b c . (a, b) t -> (b, c) t -> (a, c) t =
  fun r1 r2 -> match (r1, r2) with
    | (Nope, Nope) -> Nope
    | _ -> let withReact rx next offer =
             (commit r1).withReact rx (pipe r2 next) offer
           in Reagent { withReact }

(* TODO: I think the build should be concurrent.                            *)
(*       Humm wait it depend on what you want cf attempt reagent.           *)
let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let withReact rx next offer =
    (commit r1).withReact rx next offer ;
    (commit r2).withReact rx next offer ;
  in Reagent { withReact }

let constant (x:'a) : ('b, 'a) t =
  let withReact rx next offer =
    (commit next).withReact (rx >> return x) Nope offer
  in Reagent { withReact }

let never : ('a, 'b) t =
  let withReact _ _ _ = ()
  in Reagent { withReact }

(* this one in not equivalent to Nope, it has a next. *)
let noop : ('a, 'a) t =
  let withReact rx next offer =
    (commit next).withReact rx Nope offer
  in Reagent { withReact }


(* read ref should not be a reagent. Indeend it can *)
(* be confusing when piping as read ref is truly    *)
(* equivalent to const (!r)                         *)

let cas (r:'a casref) (updt:'a casupdt) : (unit, unit) t =
  let withReact rx next offer =
    (commit next).withReact (rx >> Reaction.cas (r <:= updt)) Nope offer
  in Reagent { withReact }
(* Hmm but the actual cas will not be performed until the commit phase..    *)
(* Yep! But this is not what reagents are for: composing actions atomically *)

let post_commit (f:'a -> unit) : ('a, 'a) t =
  let withReact rx next offer =
    let pc = (fun () -> f (Reaction.get_value rx)) in
    (commit next).withReact (Reaction.pc pc >> rx) Nope offer
  in Reagent { withReact }

(* f is total for the moment (contrary to the scala version of lift) *)
(* TODO: when trasient failure, partial, with option and / or exn    *)
(*                                                                   *)
(* Be careful with lift and computed, their behaviour can be non-    *)
(* intuistic when sequenced. Remember that a reaction is two-phased. *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let withReact rx next offer =
    (commit next).withReact (Reaction.map f rx) Nope offer
  in Reagent { withReact }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let withReact rx next offer =
    (commit (f (Reaction.get_value rx))).withReact (rx >> return ()) next offer
  in Reagent { withReact }

(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction might be concurrent... (not as in the scala version!)    *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let withReact rx next offer =
    let (a, c) = Reaction.get_value rx in
    (commit (pipe r (lift (fun b -> (b, c))))).withReact (rx >> return a) next offer
  in Reagent { withReact }

let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let withReact rx next offer =
    let (c, a) = Reaction.get_value rx in
    (commit (pipe r (lift (fun b -> (c, b))))).withReact (rx >> return a) next offer
  in Reagent { withReact }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  pipe (lift (fun a -> (a, a)))
       (pipe (first r1) (second r2))


module Sugar = struct
  let ( * ) = pair
  let ( || ) = choose
  let ( |> ) = pipe
end
