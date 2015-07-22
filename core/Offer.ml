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
  type 'a t
  val catalist : unit t
  val is_waiting : 'a t -> bool
  val rx_complete : 'a t -> 'a -> unit Reaction.t
  val rx_has : 'a Reaction.t -> 'b t -> bool
  (* TODO: document, internal, only on non blocking, *)
  (* return None only when waiting and Some when     *)
  (* completing successfully.                        *)
  val try_get_result : 'a t -> 'a option
  val post_and_suspend : ('a t -> unit) -> 'a
  val post_and_return : ('a t -> unit) -> 'a t
  (* val try_resume : 'a t -> 'a -> bool *)
end

module Make (Sched : Scheduler.S) : S = struct

  open CAS.Sugar
  open Reaction.Sugar

  (* TODO: GADT to ensure that dummy is a unit status? *)
  type 'a status =
    | Dummy (* TODO: GADT to ensure that it is a unit one *)
    | New
    | Waiting of 'a Sched.cont
    | Answered of 'a
    | ToBeWaken of 'a * 'a Sched.cont (* optimisation purpose *)
    | Garbage (* TODO : use that for removable catalysits *)

  (* The thread is suposed to be resumed only once, and when    *)
  (* the message has been completed.                            *)
  (* Also, all completed offers should be waken at some point.  *)

  (* TODO: change that /\ to something explaning the new invariants... *)

  type 'a t = 'a status casref


  let create () = ref New

  let is_available o = match !o with
    | Waiting  _ | New         | Dummy   -> true
    | Answered _ | ToBeWaken _ | Garbage -> false


  let extract_value o = let s = !o in match s with
    | Answered a when (o <!= s --> Garbage) -> a
    (* TODO: this cas cause useless overhead but checks things *)
    | _          -> failwith "Error TODO 1"


  (* TODO: rx! or reagent! *)
  let wait_for o =
    let suspend () =
      Sched.suspend
        (fun k -> (* TODO: ensure by typing that the offer is yours  *)
                  (* and NOT a catalyst!                             *)
                  (* Hmm.. That seems difficult without linear types *)
                  (* TODO: check if it is completed...               *)
                  if not (o <!= New --> Waiting k) then
                    Sched.resume k (extract_value o)) in
    let s = !o in match s with
      | New   -> suspend ()
      | _     -> extract_value o (* this cover all cases of error, subtle... *)


  (* for the sake of optimisation, complete is not wake *)
  let rx_complete o a =
    let wake () =
      let s = !o in match s with
        | Dummy -> ()
        | Answered -> ()
        | ToBeWaken (x, k) when (o <!= s --> Garbage) -> Sched.resume k x
        (* TODO: this cas cause useless overhead but checks things *)
        | _     -> failwith "Offer.rx_complete: broken invariant."
    in match !o with
       | Dummy -> rx_return ()
       | (* PB : cannot provide one cas... *)


  let rx_complete o a =
    let wake x () =
    in match !(o.state) with
    | Dummy -> rx_return ()
    | _ -> Reaction.cas (o.state <:= Waiting --> Completed a)
        >> Reaction.pc (wake o)

  (* let rx_has rx o = Reaction.has_cas_on rx o.state *)
  (*  *)
  (* let try_get_result o = *)
  (*   let () = if not (o.thread == None) then *)
  (*     raise (Invalid_argument "Offer.try_get_result: \ *)
  (*       trying to get the result of a blocking offer") *)
  (*   in *)
  (*   let s = !(o.state) in *)
  (*   match s with *)
  (*   | Completed v when (o.state <!= s --> WakenUp) *)
  (*             -> Some v *)
  (*   | Waiting -> None *)
  (*   | _       -> raise (Invalid_argument "Offer.try_get_result: \ *)
  (*                  trying to get the result of an already waken offer") *)
  (*  *)
  (* let post_and_suspend f = *)
  (*   Sched.suspend (fun k -> f { state = ref Waiting ; thread = Some k }) *)
  (*  *)
  (* let post_and_return f = *)
  (*    let o = { state = ref Waiting ; thread = None } in *)
  (*    let () = Sched.fork (fun () -> f o) in *)
  (*    o *)
  (*

  let try_resume o a =
    if CAS.commit (complete_cas o a) then ( wake o () ; true )
    else false

   *)

end
