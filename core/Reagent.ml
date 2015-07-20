(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


module type S = sig
  type ('a, 'b) t
  val run : ('a, 'b) t -> 'a -> 'b
  val dissolve : (unit, unit) t -> unit
  val pipe : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val choose : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val constant : 'a -> ('b, 'a) t
  val never : ('a, 'b) t
  val retry : ('a, 'b) t
  val noop : ('a, 'a) t
  val lift : ('a -> 'b) -> ('a, 'b) t
  val lift_partial : ('a -> 'b option) -> ('a, 'b) t
  val attempt : ('a, 'b) t -> ('a, 'b option) t
  val computed : ('a -> (unit, 'b) t) -> ('a, 'b) t
  val cas : 'a CAS.ref -> 'a CAS.updt -> (unit, unit) t
  val update : 'a CAS.ref -> (('a * 'b) -> ('a * 'c) option) -> ('b, 'c) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val pair : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  type ('a, 'b) message
  val is_message_available : ('a, 'b) message -> bool
  val send : (('a, 'b) message -> unit) -> ('a, 'b) t
  val answer : ('a, 'b) message -> ('b, 'a) t
  module Sugar : sig
    val ( >*> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
    val ( >+> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  end
end


module Make (Sched : Scheduler.S) : S = struct
  module Offer = Offer.Make (Sched)

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
  type optim_flags = {
      alwaysCommit : bool ; (* Means that does not cause Retry or Block on   *)
                            (* on its own. But these can still occur due to  *)
                            (* the next or the ongoing reaction...           *)
                            (* Especially alwaysCommit = true implies that   *)
                            (* no CASes will be performed.                   *)
    }
  (* just a wrapper for the universal quantification *)
  and ('a, 'b) react_fun = { apply : 'r . 'a Reaction.t -> ('b, 'r) t -> 'r result }
  (* This GADT introduce some heavy type annotations but *)
  (* is necessary for the type system to accept what's   *)
  (* going on when commiting.                            *)
  and (_, _) t =
    | Nope : ('a, 'a) t
    | Reagent : (('a, 'b) react_fun * optim_flags) -> ('a, 'b) t

  let always_commit (type a) (type b) (r:(a, b) t) : bool = match r with
    | Nope -> true
    | Reagent (_, { alwaysCommit }) -> alwaysCommit

  let rec try_react : type a b . (a, b) t -> (a, b) react_fun = fun r -> match r with
    | Nope -> let apply (type c) (type r) (rx:c Reaction.t)
                            (next:(c, r) t) : r result = match next with
                | Nope -> if !! rx then Imm (rx_value rx) else Retry
                | _    -> (try_react next).apply rx Nope
              in { apply }
    | Reagent (r, _) -> r

  (*** CORE COMBINATORS ***)

  (* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
  (* and the channel is empty: it will block ; should it atomically exchange? *)
  (* what does the scalal version?                                            *)
  let rec pipe : type a b c . (a, b) t -> (b, c) t -> (a, c) t =
    fun r1 r2 -> match (r1, r2) with
      | (Nope, Nope) -> Nope
      | _ -> let apply rx next =
               (try_react r1).apply rx (pipe r2 next)
             in Reagent ({ apply }, { alwaysCommit =    always_commit r1
                                                     && always_commit r2  })

  (* TODO: Also write a concurrent (and thus symetric) choose.                *)
  let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
    let apply rx next = match (try_react r1).apply rx next with
      | Imm x1 -> Imm x1
      | Retry ->
         begin match (try_react r2).apply rx next with
               | Imm x2 -> Imm x2
               | Retry -> Retry
               | Block g
               | BlockOrRetry g -> BlockOrRetry g
         end
      | Block f ->
         begin match (try_react r2).apply rx next with
               | Imm x2 -> Imm x2
               | Retry -> BlockOrRetry f
               | Block g -> Block (fun o -> f o ; g o)
               | BlockOrRetry g -> BlockOrRetry (fun o -> f o ; g o)
         end
      | BlockOrRetry f ->
         begin match (try_react r2).apply rx next with
               | Imm x2 -> Imm x2
               | Retry -> BlockOrRetry f
               | Block g
               | BlockOrRetry g -> BlockOrRetry (fun o -> f o ; g o)
         end
    in Reagent ({ apply }, { alwaysCommit =    always_commit r1
                                            && always_commit r2  })
               (* it seems it has to be && and not || to      *)
               (* ensure atomicity when a cas occur in one    *)
               (* branch of the choice...                     *)

  let constant (x:'a) : ('b, 'a) t =
    let apply rx next =
      (try_react next).apply (rx >> rx_return x) Nope
    in Reagent ({ apply }, { alwaysCommit = true })

  let never : ('a, 'b) t =
    let apply _ _ = Block (fun _ -> ())
    in Reagent ({ apply }, { alwaysCommit = false })

  let retry : ('a, 'b) t =
    let apply _ _ = Retry
    in Reagent ({ apply }, { alwaysCommit = false })

  (* this one in not equivalent to Nope, it has a next. *)
  let noop : ('a, 'a) t =
    let apply rx next =
      (try_react next).apply rx Nope
    in Reagent ({ apply }, { alwaysCommit = true })


  (* Be careful with lift and computed, their behaviour can be non-    *)
  (* intuistic when sequenced. Remember that a reaction is two-phased. *)
  (* f should be total                                                 *)
  let lift (f:'a -> 'b) : ('a, 'b) t =
    let apply rx next =
      (try_react next).apply (Reaction.map f rx) Nope
    in Reagent ({ apply }, { alwaysCommit = true })

  let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
    let apply rx next =
      (try_react (f (rx_value rx))).apply (rx >> rx_return ()) next
    in Reagent ({ apply }, { alwaysCommit = false })


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
    let apply rx next =
      if always_commit next && Reaction.count_cas rx = 0 then
        (* it is safe not to add the cas to the reaction *)
        (* because we know no cas is going to conflict   *)
        if CAS.commit (r <:= updt) then (try_react next).apply rx Nope
        else Retry
      else
        if Reaction.has_cas_on rx r then Block (fun _ -> ())
        else (try_react next).apply (rx >> Reaction.cas (r <:= updt)) Nope
    in Reagent ({ apply }, { alwaysCommit = false })
  (* Hmm but the actual cas will not be performed until the commit phase..    *)
  (* So a reaction with two cas on the same value will always fail.           *)
  (* Yep! But this is not what reagents are for: composing actions atomically *)
  (* TODO: document that it will block indefinetly if the cas is already part *)
  (* of the reaction. This is to be used with attemps/choose.                 *)

  (* TODO: document that it blocks when None *)
  let update (r:'a casref) (f:('a * 'b) -> ('a * 'c) option) : ('b, 'c) t =
    computed (fun b -> let a = !r in
                       match f (a, b) with
                       | None -> never
                       | Some (a', c) -> pipe (cas r (a --> a')) (constant c))

  let post_commit (f:'a -> unit) : ('a, 'a) t =
    let apply rx next =
      let pc = (fun () -> f (rx_value rx)) in
      (try_react next).apply (Reaction.pc pc >> rx) Nope
    in Reagent ({ apply }, { alwaysCommit = true })

  (* TODO: I am not fully satistied with the pair, I think the built up of *)
  (* the reaction might be concurrent... (not as in the scala version!)    *)
  let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
    let apply rx next =
      let (a, c) = rx_value rx in
      (try_react (pipe r (lift (fun b -> (b, c))))).apply (rx >> rx_return a) next
    in Reagent ({ apply }, { alwaysCommit = always_commit r })

  let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
    let apply rx next =
      let (c, a) = rx_value rx in
      (try_react (pipe r (lift (fun b -> (c, b))))).apply (rx >> rx_return a) next
    in Reagent ({ apply }, { alwaysCommit = always_commit r })

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
    let apply rx next =
      Block (fun offer -> f (M { senderRx = rx ; senderK = next ; offer = offer }))
    in Reagent ({ apply }, { alwaysCommit = false })

  (* TODO: document that it will block indefinetly if the offer of *)
  (* the message is already is part of the reaction cf cas.        *)
  let answer (M m) =
    let merge =
      let apply rx next =
        if Offer.rx_has rx m.offer then Block (fun _ -> ())
        else
          (try_react next).apply ( rx >> Offer.rx_complete m.offer (rx_value rx)
                                      >> m.senderRx )
                                  (* The other reagent is given Reaction.inert,    *)
                                  (* it is this one's role to enforce the whole    *)
                                  (* reaction (i.e. both reactions and the         *)
                                  (* message passing).                             *)
                                  Nope
      in Reagent ({ apply }, { alwaysCommit = false })
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
    >>> computed (fun _ -> match Offer.try_get_result offer with
                  | None -> retry
                  | Some x -> constant x)

  let run (r:('a, 'b) t) (arg:'a) : 'b =
    let b = Backoff.create () in
    let wait () =
      (* Sched.yield () *)
      Backoff.once b
    in
    let rec retry_loop : 'c . ('a, 'c) t -> 'c = (fun r ->
      match (try_react r).apply (rx_return arg) Nope with
        | Imm x -> x
        | Retry -> ( wait () ; retry_loop r )
        | Block f -> Offer.post_and_suspend f
        | BlockOrRetry f ->
           let o = Offer.post_and_return f in
           retry_loop (retry_with r o)
    )  in retry_loop r

  let dissolve (r:(unit, unit) t) : unit =
    match (try_react (r >>> never)).apply (rx_return ()) Nope with
    | Imm _ | Retry | BlockOrRetry _ ->
       raise (Invalid_argument "Reagent.dissolve: \
         only blocking reagent are supposed to be called with dissolve")
    | Block f -> f Offer.catalist

end
