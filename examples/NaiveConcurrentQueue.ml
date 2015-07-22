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
  type ('a, 'b) reagent

  type 'a t
  val create : unit -> 'a t
  val push : 'a t -> ('a, unit) reagent
  val pop : 'a t -> (unit, 'a option) reagent
  val pop_until : 'a t -> ('a -> bool) -> (unit, unit) reagent
  type 'a cursor
  val snapshot : 'a t -> 'a cursor
  val next : 'a cursor -> ('a * 'a cursor) option
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open CAS.Sugar
  open Reagent.Sugar

  (* Concurrent queue based on functionnal queue stored in a CAS reference.
   * Not the most efficient concurrent queue, but has avantages, like returning
   * the whole content without copying it! This queue is non-blocking, it only
   * fail trasiently.
   *)

  type 'a t = 'a FQueue.t casref

  let create () = ref FQueue.empty

  let push q = Reagent.update q (fun (fq, v) -> (FQueue.push v fq, ()))

  let pop q = Reagent.update q (fun (fq, ()) ->
                                try let (v, nq) = FQueue.pop fq in (nq, Some v)
                                with FQueue.Empty -> (fq, None))

  let pop_until q f =
    let rec loop fq =
      try
        let (v, n) = FQueue.pop fq in
        if not (f v) then loop n
        else fq
      with FQueue.Empty -> fq
    in Reagent.update q (fun (fq, ()) -> (loop fq, ()))

  type 'a cursor = 'a FQueue.t

  let snapshot q = !q

  let next c = try Some (FQueue.pop c) with FQueue.Empty -> None

end
