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
  module Channel = Channel.Make (Sched) (* TODO: should use ReagentLib.Make? *)

  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open Reagent.Sugar
  open CAS.Sugar

  type 'a t = { queue    : 'a list casref              ;
                popchan  : (unit, 'a) Channel.endpoint ;
                pushchan : ('a, unit) Channel.endpoint }

  let create () =
    let (a, b) = Channel.create () in
    { queue = ref [] ; popchan = a ; pushchan = b }

  let push s =
        Reagent.update s.queue (fun (l, a) -> (a :: l, ()))
    >+> Channel.swap s.pushchan

  let tryPop s =
        Reagent.update s.queue (fun (l, ()) -> match l with
                                       | [] -> ([], None)
                                       | h :: t -> (t, Some h))
    >+> (Channel.swap s.popchan >>> Reagent.lift (fun x -> Some x))

end
