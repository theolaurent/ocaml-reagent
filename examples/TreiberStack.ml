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
  val tryPop : 'a t -> (unit, 'a option) reagent
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open CAS.Sugar
  open Reagent.Sugar

  type 'a t = { head : 'a list casref }

  let create () = {head = CAS.Sugar.ref []}

  let push stack = Reagent.update stack.head (fun (s, v) -> (v :: s, ()))

  (* TODO: should use update reagent? *)
  let tryPop stack =
    Reagent.update stack.head (fun (s, ()) -> match s with
                               | [] -> (s, None)
                               | h :: t -> (t, Some h))

end
