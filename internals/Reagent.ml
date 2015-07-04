
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim when only one cas                       *)

type 'a result =
  | Imm of 'a Reaction.t
  | Block of ('a Reaction.t Offer.t -> unit)
  | Retry of ('a Reaction.t Offer.t -> unit) option

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
              | Nope -> Imm rx
              | _    -> (commit next).withReact rx Nope
            in { withReact }
  | Reagent r -> r



let run (r:('a, 'b) t) (arg:'a) : 'b =
  let wait () = (* TODO: exponential wait *)
    perform Sched.Yield
  in
  let rec retry_offer_loop offer =
    match (commit r).withReact (rx_return arg) Nope with
    | Imm rx  -> ignore (Offer.try_resume offer rx)
    | Block f -> f offer
    | Retry None
    | Retry (Some _) -> (* for space complexity reasons, the offer is *)
                        (* assumed to be already posted               *)
                        ( wait () ; retry_offer_loop offer )
  in
  let rec retry_loop () =
    let rx = match (commit r).withReact (rx_return arg) Nope with
      | Imm rx -> Some rx
      | Block f -> Some (Offer.suspend f)
      | Retry None -> None
      | Retry Some f ->
         (* TODO: this is a hack, do things properly *)
         Some (Offer.suspend
                 (fun o -> f o ; perform (Sched.Fork
                                            (fun () -> retry_offer_loop o))))
    in match rx with
       | Some rx when !! rx -> rx_value rx
       | _ -> ( wait () ; retry_loop () )
  in retry_loop ()


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
    | Imm rx1 -> Imm rx1
    | Block f ->
       begin match (commit r2).withReact rx next with
             | Imm rx2 -> Imm rx2
             | Block g -> Block (fun o -> f o ; g o)
             | Retry (Some g) -> Retry (Some (fun o -> f o ; g o))
             | Retry None -> Retry (Some f)
       end
    | Retry (Some f) ->
       begin match (commit r2).withReact rx next with
             | Imm rx2 -> Imm rx2
             | Block g
             | Retry (Some g) -> Retry (Some (fun o -> f o ; g o))
             | Retry None -> Retry (Some f)
       end
    | Retry None ->
       begin match (commit r2).withReact rx next with
             | Imm rx2 -> Imm rx2
             | Block g
             | Retry (Some g) -> Retry (Some g)
             | Retry None -> Retry None
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
  let withReact _ _ = Retry None
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
    (commit next).withReact (rx >> Reaction.cas (r <:= updt)) Nope
  in Reagent { withReact }
(* Hmm but the actual cas will not be performed until the commit phase..    *)
(* So a reaction with two cas on the same value will always fail.           *)
(* Yep! But this is not what reagents are for: composing actions atomically *)

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


type ('a, 'b, 'r) message_struct = { senderRx : 'a Reaction.t ;
                                     senderK  : ('b, 'r) t    ;
                                     offer    : 'r Reaction.t Offer.t    }
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
(* the message is already is part of the reaction.               *)
(* This is to be used with attemps/choose.                       *)
let answer (M m) =
  let merge =
    let withReact rx next =
      if Reaction.has_offer rx m.offer then Block (fun _ -> ())
      else
        (commit next).withReact ( rx >> Reaction.completion m.offer (Reaction.clear rx)
                                     >> m.senderRx )
                                (* The other reagent is given Reaction.inert,    *)
                                (* it is this one's role to enforce the whole    *)
                                (* reaction (i.e. both reactions and the         *)
                                (* message passing).                             *)
                                Nope
    in Reagent { withReact }
  in pipe m.senderK merge



(* TODO: these are really bad choices, they overlap with Pervasives' operators *)
module Sugar = struct
  let ( * ) = pair
  let ( || ) = choose
  let ( |> ) = pipe
end
