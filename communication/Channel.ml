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

  type ('a, 'b) endpoint
  val create : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
  val swap : ('a, 'b) endpoint -> ('a, 'b) reagent
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  module MSQueue = MSQueue.Make (Sched)
  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open Reagent.Sugar

  (* TODO: what is the expected behaviour when posting on both *)
  (* sides of a channel?                                       *)

  type ('a, 'b) endpoint = { outgoing : ('a, 'b) Reagent.message MSQueue.t ;
                             incoming : ('b, 'a) Reagent.message MSQueue.t }

  let flip_endpoint e = { outgoing = e.incoming ;
                          incoming = e.outgoing }

  let create () =
    let e1 = { outgoing = MSQueue.create () ;
               incoming = MSQueue.create () } in
    (e1, flip_endpoint e1)


  let swap e =
    let () = Reagent.run
               (MSQueue.pop_until e.incoming
                                  Reagent.is_message_available) () in
    let rec all_messages c =
      match MSQueue.next c with
      | None -> Reagent.never
      | Some (m, nc) -> (* use Reagent.computed to get lazyness! *)
         Reagent.computed (fun a -> Reagent.constant a >>> (    Reagent.answer m
                                                            >+> all_messages nc  ))
    in
    let push_message m = Reagent.run (MSQueue.push e.outgoing) m in
    all_messages (MSQueue.snapshot e.incoming) >+> Reagent.send push_message

end
