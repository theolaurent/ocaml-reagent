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
  val catalyst          : unit t
  val post_and_suspend  : ('a t -> unit) -> 'a
  val post_and_return   : ('a t -> unit) -> 'a t

  val is_waiting        : 'a t -> bool
  val rx_complete       : 'a t -> 'a -> unit Reaction.t
  val rx_has            : 'a Reaction.t -> 'b t -> bool
  val try_get_result    : 'a t -> 'a option
  (* val try_resume : 'a t -> 'a -> bool *)
end

module Make (Sched : Scheduler.S) : S = struct

  open CAS.Sugar
  open Reaction.Sugar

  (* TODO: GADT to ensure that dummy is a unit status? *)
  type 'a status =
    | Waiting
    | Completed of 'a
    | Wokenup
    | Catalyst

  (* The thread is suposed to be resumed only once, and when the message has
   * been completed. Also, all completed offers should be woken at some point.
   * *)
  type 'a t = { state  : 'a status casref     ;
                thread : 'a Sched.cont option }


  (* TODO : removable catalysts *)
  let catalyst = { state = ref Catalyst ; thread = None }

  let is_waiting o = match !(o.state) with
    | Waiting | Catalyst -> true
    | _               -> false

  let rx_complete o a =
    let wake x () =
      let s = !(x.state) in
      match s with
      | Completed v when (x.state <!= s --> Wokenup)
              -> begin match x.thread with
                       | None -> ()
                       | Some t -> Sched.resume t v
                 end
      | _     -> raise (Invalid_argument "Offer.rx_complete: \
                   Trying to wakeup an incomplete or already woken up offer")
    in match !(o.state) with
    | Catalyst -> rx_return ()
    | _ -> Reaction.cas (o.state <:= Waiting --> Completed a)
        >> Reaction.pc (wake o)

  let rx_has rx o = Reaction.has_cas_on rx o.state

  let try_get_result o =
    let () = if not (o.thread == None) then
      raise (Invalid_argument "Offer.try_get_result: \
        trying to get the result of a blocking offer")
    in
    let s = !(o.state) in
    match s with
    | Completed v when (o.state <!= s --> Wokenup)
              -> Some v
    | Waiting -> None
    | _       -> raise (Invalid_argument "Offer.try_get_result: \
                   trying to get the result of an already waken offer")

  let post_and_suspend f =
    Sched.suspend (fun k -> f { state = ref Waiting ; thread = Some k })

  let post_and_return f =
     let o = { state = ref Waiting ; thread = None } in
     let () = Sched.fork (fun () -> f o) in
     o

  (*

  let try_resume o a =
    if CAS.commit (complete_cas o a) then ( wake o () ; true )
    else false

   *)

end
