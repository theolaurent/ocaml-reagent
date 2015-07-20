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
  val exchange : 'a t -> ('a, 'a) reagent
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  module Channel = Channel.Make (Sched) (* TODO: should use ReagentLib.Make? *)

  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open Reagent.Sugar

  type 'a t = (('a, 'a) Channel.endpoint * ('a, 'a) Channel.endpoint)

  let create = Channel.create

  (* TODO: it would be better with a symmetric choose? *)
  let exchange e = Channel.swap (fst e) >+> Channel.swap (snd e)

end
