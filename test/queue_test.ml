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

module ReagentLib = ReagentLib.Make (Sched)
open ReagentLib

open Printf

module type QUEUE = sig
  type 'a t
  val create : unit -> 'a t
  val push   : 'a t -> 'a -> unit
  val pop    : 'a t -> 'a option
end

module type SCHED = sig
  val fork  : (unit -> unit) -> unit
  val yield : unit -> unit
end

module FunQ : QUEUE = struct

  module N = NaiveConcurrentQueue.Make (Sched)

  type 'a t = 'a N.t

  let create = N.create
  let push q v = Reagent.run (N.push q) v
  let pop q = Reagent.run (N.pop q) ()
end

module Benchmark = struct
  let get_mean_sd l =
    let get_mean l = (List.fold_right (fun a v -> a +. v) l 0.) /.
                (float_of_int @@ List.length l)
    in
    let mean = get_mean l in
    let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
    (mean, sd)

  let benchmark f n =
    let rec run acc = function
    | 0 -> acc
    | n -> let t1 = Sys.time () in
          let () = f () in
          let d = Sys.time () -. t1 in
          run (d::acc) (n-1)
    in
    let r = run [] n in
    get_mean_sd r
end

module Test (Q : QUEUE) = struct

  let q : int Q.t = Q.create ()

  let run num_doms num_items =
    (* initialize work *)
    let rec produce = function
      | 0 -> printf "[%d] production complete\n%!" (Domain.self ())
      | i -> Q.push q i; produce (i-1)
    in
    let rec consume i =
      match Q.pop q with
      | None -> printf "[%d] consumed=%d\n%!" (Domain.self ()) i
      | Some _ -> consume (i+1)
    in
    for i = 1 to num_doms - 1 do
      Domain.spawn (fun () -> produce num_items; consume 0)
    done;
    produce num_items;
    consume 0
end

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_domains> <num_items>";
  exit(0)

let (num_doms, num_items) =
  if Array.length Sys.argv < 3 then
    print_usage_and_exit ()
  else
    try
      let a = int_of_string (Sys.argv.(1)) in
      let b = int_of_string (Sys.argv.(2)) in
      (a,b)
    with
    | Failure _ -> print_usage_and_exit ()

module M1 = Test(FunQ)
let (m,sd) = Benchmark.benchmark (fun () -> M1.run num_doms num_items) 1
let () = printf "FunQ: mean = %f, sd = %f\n%!" m sd
