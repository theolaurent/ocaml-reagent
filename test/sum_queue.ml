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

module ReagentLib = ReagentLib.Make (Sched_sq)
open ReagentLib

open Reagent.Sugar

let q = MSQueue.create ()

let nb_workers = 10

let (post, get) = Channel.create ()

let step = 1000

let enqueue_work i =
  for j = (i + 1) * step to (i + 2) * step - 1 do
    let x = j - step in
    Printf.printf "Domain [%d], Worker [%d]: Enqueuing %d\n%!" (Domain.self ()) i x ;
    Reagent.run (MSQueue.push q) x
  done

let sum_work i =
  let rec loop sum = match Reagent.run (MSQueue.pop q) () with
    | None -> sum
    | Some x ->
       Printf.printf "Domain [%d], Worker [%d]: Dequeuing %d\n%!" (Domain.self ()) i x ;
       loop (sum + x)
  in let sum = loop 0 in
     Printf.printf "Domain [%d], Worker [%d]: Posting %d\n%!" (Domain.self ()) i sum ;
     Reagent.run (Channel.swap post) sum

let main () =
  Printf.printf "Enter main\n%!" ;
  for i = 0 to nb_workers - 1 do
    Printf.printf "Forking worker %d\n%!" i ;
    Sched_sq.fork (fun () -> enqueue_work i ; sum_work i)
  done ;
  Printf.printf "Fetching sums\n%!" ;
  let sum () =
    let a = Array.init nb_workers (fun _ -> Reagent.run (Channel.swap get) ()) in
    Array.fold_left (+) 0 a
  in
  Printf.printf "Sum     : %d\n%!" (sum ()) ;
  Printf.printf "Expected: %d\n%!" (let n = step * nb_workers - 1 in (n * (n + 1)) / 2)

let () = Sched_sq.run main
