
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim when only one cas                       *)

type 'a result =
  | Imm of 'a
  | Retry
  | Block of ('a Offer.t -> unit)
  | BlockOrRetry of ('a Offer.t -> unit)
(* When blocking/retrying, the offer should always be embedded in a message *)
(* or completely ignored, but it should never be fulfilled without taking   *)
(* care of the reaction.                                                    *)

(* TODO: variance *)
type ('a, 'b) t_struct = {
    withReact : 'r . 'a Reaction.t -> ('b, 'r) t -> 'r result
  }
(* This GADT introduce some heavy type annotations but *)
(* is necessary for the type system to accept what's   *)
(* going on when commiting.                            *)
and (_, _) t =
  | Nope : ('a, 'a) t
  | Reagent : ('a, 'b) t_struct -> ('a, 'b) t

let rec commit : type a b . (a, b) t -> (a, b) t_struct = fun r -> match r with
  | Nope -> let withReact (type c) (type r) (rx:c Reaction.t)
                          (next:(c, r) t) : r result = match next with
              | Nope -> if !! rx then Imm (rx_value rx) else Retry
              | _    -> (commit next).withReact rx Nope
            in { withReact }
  | Reagent r -> r

(*** CORE COMBINATORS ***)

(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let rec pipe : type a b c . (a, b) t -> (b, c) t -> (a, c) t =
  fun r1 r2 -> match (r1, r2) with
    | (Nope, Nope) -> Nope
    | _ -> let withReact rx next =
             (commit r1).withReact rx (pipe r2 next)
           in Reagent { withReact }

(* TODO: Also write a concurrent (and thus symetric) choose.                *)
let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let withReact rx next = match (commit r1).withReact rx next with
    | Imm x1 -> Imm x1
    | Retry ->
       begin match (commit r2).withReact rx next with
             | Imm x2 -> Imm x2
             | Retry -> Retry
             | Block g
             | BlockOrRetry g -> BlockOrRetry g
       end
    | Block f ->
       begin match (commit r2).withReact rx next with
             | Imm x2 -> Imm x2
             | Retry -> BlockOrRetry f
             | Block g -> Block (fun o -> f o ; g o)
             | BlockOrRetry g -> BlockOrRetry (fun o -> f o ; g o)
       end
    | BlockOrRetry f ->
       begin match (commit r2).withReact rx next with
             | Imm x2 -> Imm x2
             | Retry -> BlockOrRetry f
             | Block g
             | BlockOrRetry g -> BlockOrRetry (fun o -> f o ; g o)
       end
  in Reagent { withReact }

let constant (x:'a) : ('b, 'a) t =
  let withReact rx next =
    (commit next).withReact (rx >> rx_return x) Nope
  in Reagent { withReact }

let never : ('a, 'b) t =
  let withReact _ _ = Block (fun _ -> ())
  in Reagent { withReact }

let retry : ('a, 'b) t =
  let withReact _ _ = Retry
  in Reagent { withReact }

(* this one in not equivalent to Nope, it has a next. *)
let noop : ('a, 'a) t =
  let withReact rx next =
    (commit next).withReact rx Nope
  in Reagent { withReact }


(* Be careful with lift and computed, their behaviour can be non-    *)
(* intuistic when sequenced. Remember that a reaction is two-phased. *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let withReact rx next =
    (commit next).withReact (Reaction.map f rx) Nope
  in Reagent { withReact }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let withReact rx next =
    (commit (f (rx_value rx))).withReact (rx >> rx_return ()) next
  in Reagent { withReact }


let attempt (r:('a, 'b) t) : ('a, 'b option) t =
  choose (pipe r (lift (fun x -> Some x))) (constant None)

(* if you want retry version, use this one with choose retry! *)
let lift_partial (f:'a -> 'b option) : ('a, 'b) t =
  computed (fun a -> match f a with
                     | None -> never
                     | Some x -> constant x)


(* I don't know if read should be a reagent. It is       *)
(* trivialy written, and anyway rarelly used on its own. *)
(* let read (r:'a casref) : (unit, 'a) t =               *)
(*   lift (fun () -> !r)                                 *)
(* As a matter of facts, I think read action in          *)
(* interfaces should never be reagents.                  *)

let cas (r:'a casref) (updt:'a casupdt) : (unit, unit) t =
  let withReact rx next =
    let cas = (r <:= updt) in
    if Reaction.has_cas rx cas then Block (fun _ -> ())
    else (commit next).withReact (rx >> Reaction.cas cas) Nope
  in Reagent { withReact }
(* Hmm but the actual cas will not be performed until the commit phase..    *)
(* So a reaction with two cas on the same value will always fail.           *)
(* Yep! But this is not what reagents are for: composing actions atomically *)
(* TODO: document that it will block indefinetly if the cas is already part *)
(* of the reaction. This is to be used with attemps/choose.                 *)

let post_commit (f:'a -> unit) : ('a, 'a) t =
  let withReact rx next =
    let pc = (fun () -> f (rx_value rx)) in
    (commit next).withReact (Reaction.pc pc >> rx) Nope
  in Reagent { withReact }

(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction might be concurrent... (not as in the scala version!)    *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let withReact rx next =
    let (a, c) = rx_value rx in
    (commit (pipe r (lift (fun b -> (b, c))))).withReact (rx >> rx_return a) next
  in Reagent { withReact }

let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let withReact rx next =
    let (c, a) = rx_value rx in
    (commit (pipe r (lift (fun b -> (c, b))))).withReact (rx >> rx_return a) next
  in Reagent { withReact }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  pipe (lift (fun a -> (a, a)))
       (pipe (first r1) (second r2))


module Sugar = struct
  let ( >*> ) = pair
  let ( >+> ) = choose
  let ( >>> ) = pipe
end

open Sugar

(*** MESSAGE PASSING ***)

type ('a, 'b, 'r) message_struct = { senderRx : 'a Reaction.t ;
                                     senderK  : ('b, 'r) t    ;
                                     offer    : 'r Offer.t    }
type (_, _) message =
  M : ('a, 'b, 'r) message_struct -> ('a, 'b) message

let is_message_available (M m) =
  Offer.is_waiting m.offer


(* send is always blocking, it is to be used together with choose/atempt *)
let send f =
  let withReact rx next =
    Block (fun offer -> f (M { senderRx = rx ; senderK = next ; offer = offer }))
  in Reagent { withReact }

(* TODO: document that it will block indefinetly if the offer of *)
(* the message is already is part of the reaction cf cas.        *)
let answer (M m) =
  let merge =
    let withReact rx next =
      let cas = Offer.complete_cas m.offer (rx_value rx) in
      if Reaction.has_cas rx cas then Block (fun _ -> ())
      else
        (commit next).withReact ( rx >> Reaction.cas cas >> Reaction.pc (Offer.wake m.offer)
                                     >> m.senderRx )
                                (* The other reagent is given Reaction.inert,    *)
                                (* it is this one's role to enforce the whole    *)
                                (* reaction (i.e. both reactions and the         *)
                                (* message passing).                             *)
                                Nope
    in Reagent { withReact }
  in m.senderK >>> merge


(*** RUNNING REAGENTS ***)

(* This is an internal reagent intended for specific use. It pipe its argument *)
(* r to aswering the offer. If the whole thing succeed, or if the offer has    *)
(* been fulfilled by someone else, it returns unit. Otherwise, it returns      *)
(* Retry, ignoring blocking from r.                                            *)
let retry_with r offer =
      attempt (r >>> answer (M { senderRx = rx_return () ;
                                 senderK  = Nope         ;
                                 offer    = offer        }))
  >>> computed (function
                 | None when Offer.is_waiting offer -> retry
                 | _ -> constant ())


let run (r:('a, 'b) t) (arg:'a) : 'b =
  let wait () = (* TODO: exponential wait *)
    perform Sched.Yield
  in
  let rec retry_loop : 'c . ('a, 'c) t -> 'c = (fun r ->
    match (commit r).withReact (rx_return arg) Nope with
      | Imm x -> x
      | Retry -> ( wait () ; retry_loop r )
      | Block f -> Offer.suspend f
      | BlockOrRetry f ->
         (* TODO: this fork is a hack, is there a better way? *)
         (Offer.suspend
            (fun o -> perform (Sched.Fork
                                 (fun () -> retry_loop (retry_with r o))) ;
                      f o))
  )  in retry_loop r
