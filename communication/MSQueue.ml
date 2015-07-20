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

  (* Michael-Scott queue, using reagents *)

  type 'a node =
    | Nil
    | Next of 'a * 'a node casref

  type 'a t = { head : 'a node casref ; tail : 'a node casref }
  (* the first node of the list starting a head is ignored *)
  (* the tail never point to Nil                           *)

  let create () =
    let head = (Next (Obj.magic (), ref Nil)) in
    { head = ref head ; tail = ref head }

  let pop q =
    Reagent.computed (fun () ->
      let s = !(q.head) in
      let nhead = match s with
        | Nil -> failwith "MSQueue.pop: broken invariant"
        | Next (_, x) -> !x
      in match nhead with
      | Nil -> Reagent.constant None
      | Next (v, _) -> Reagent.cas q.head (s --> nhead)
                   >>> Reagent.constant (Some v)
    )

  let push q =
    let rec find_tail_and_enq curr_end node =
          Reagent.cas curr_end (Nil --> node)
      >+> Reagent.computed (fun () -> match !curr_end with
            | Nil -> failwith "MSQueue.push: broken invariant."
            | Next (_, n) -> find_tail_and_enq n node
          )
    in
    Reagent.computed (fun v ->
      let newnode = Next (v, ref Nil) in
      match !(q.tail) with
      | Nil         -> failwith "MSQueue.push: broken invariant."
      | Next (_, n) -> find_tail_and_enq n newnode
                   >>> Reagent.post_commit
                         (* try to update tail, but ignore when fail *)
                         (fun () -> ignore (q.tail <!= !(q.tail) --> newnode))
    )

  let rec pop_until q f =
    (  pop q >>>
       Reagent.computed (fun x ->
         match x with
         | None -> Reagent.constant ()
         | Some x ->
             if f x then Reagent.never
             else Reagent.post_commit (Reagent.run (pop_until q f))
             (* use post commit to pop 1-by-1, more efficient that kCAS *))
    ) >+> Reagent.constant ()

  type 'a cursor = 'a node

  let snapshot q = match !(q.head) with
    | Nil -> failwith "MSQueue.snapshot: broken invariant"
    | Next (_, n) -> !n

  let next c = match c with
    | Nil -> None
    | Next (a, n) -> Some (a, !n)

end
