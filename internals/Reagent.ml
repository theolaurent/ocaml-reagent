
open CAS.Sugar
open Reaction.Sugar

(* TODO: optim block / non block                         *)
(* TODO: optim when only one cas                         *)

type ('a, 'b) t_struct = { withReact : 'r . 'a -> Reaction.t
                                              -> ('b, 'r) t
                                              -> 'r Offer.t -> unit ;
                         }
and ('a, 'b) t = ('a, 'b) t_struct option


(* TODO: find a way to explain what's going on to the type system and *)
(*       get rid of Obj.magic                                         *)
let rec commit : 'a 'b . ('a, 'b) t -> ('a, 'b) t_struct = fun r ->
  let commit_struct : ('c, 'c) t_struct =
    let withReact arg rx next offer = match next with
      | None -> if !! rx
                then ignore (Offer.try_resume offer (Obj.magic arg))
                else failwith "Reagent.commit: No transient failure for now"
      | _    -> (commit next).withReact arg rx None offer
    in { withReact }
  in
  match r with
  | None -> Obj.magic commit_struct
  | Some r -> r



let run : ('a, 'b) t -> 'a -> 'b =
  (fun r arg -> Offer.suspend ((commit r).withReact arg Reaction.inert None))

(* TODO: think about what happen with (swap enpoint a) >> (swap enpoint b)  *)
(* and the channel is empty: it will block ; should it atomically exchange? *)
(* what does the scalal version?                                            *)
let rec pipe : 'a 'b 'c . ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t = (fun r1 r2 ->
   let withReact arg rx next offer =
     (commit r1).withReact arg rx (match (r2, next) with (* to prevent commit to loop *)
                                   | (None, None) -> None
                                   | _ -> pipe r2 next)
                offer
   in Some { withReact }
  )

(* TODO: I think the build should be concurrent.                            *)
(*       Humm wait it depend on what you want cf tempt reagent.             *)
let choose (r1:('a, 'b) t) (r2:('a, 'b) t) : ('a, 'b) t =
  let withReact arg rx next offer =
    (commit r1).withReact arg rx next offer ; (commit r2).withReact arg rx next offer
  in Some { withReact }

let constant (x:'a) : ('b, 'a) t =
  let withReact _ rx next offer =
    (commit next).withReact x rx None offer
  in Some { withReact }

let never : ('a, 'b) t =
  let withReact _ _ _ _ = ()
  in Some { withReact }

let noop : ('a, 'a) t =
  let withReact arg rx next offer =
    (commit next).withReact arg rx None offer
  in Some { withReact }


(* read ref should not be a reagent. Indeend it can *)
(* be confusing when piping as read ref is truly    *)
(* equivalent to const (!r)                         *)

let cas (r:'a casref) (updt:'a casupdt) : (unit, unit) t =
  let withReact () rx next offer =
    (commit next).withReact () (rx ++ Reaction.cas (r <:= updt)) None offer
  in Some { withReact }
(* TODO: Hmm but the actual cas will not be performed until the commit phase ..    *)
(* Thus, two cas piped are bound to fail? (the problem being cas >> read is false! *)
(* I think this version behave as the scala code thought  ..                       *)


(* f is total for the moment (contrary to the scala version of lift) *)
(* TODO: when trasient failure, partial, with option and / or exn    *)
(*                                                                   *)
(* Be careful with lift and computed, their behaviour can be non-    *)
(* intuistic when sequenced. Remember that a reaction is two-phased. *)
let lift (f:'a -> 'b) : ('a, 'b) t =
  let withReact arg rx next offer =
    (commit next).withReact (f arg) rx None offer
  in Some { withReact }

let computed (f:('a -> (unit, 'b) t)) : ('a, 'b) t =
  let withReact arg rx next offer =
    (commit (f arg)).withReact () rx next offer
  in Some { withReact }


(* TODO: I am not fully satistied with the pair, I think the built up of *)
(* the reaction might be concurrent... (not as in the scala version!)    *)
let first (r:('a, 'b) t) : ('a * 'c, 'b * 'c) t =
  let withReact (a, c) rx next offer =
    (commit (pipe r (lift (fun b -> (b, c))))).withReact a rx next offer
  in Some { withReact }

let second (r:('a, 'b) t) : ('c * 'a, 'c * 'b) t =
  let withReact (c, a) rx next offer =
    (commit (pipe r (lift (fun b -> (c, b))))).withReact a rx next offer
  in Some { withReact }

let pair (r1:('a, 'b) t) (r2:('a, 'c) t) : ('a, ('b * 'c)) t =
  pipe (lift (fun a -> (a, a)))
       (pipe (first r1) (second r2))

module Sugar = struct
  let ( * ) = pair
  let ( + ) = choose
  let ( >> ) = pipe
end
