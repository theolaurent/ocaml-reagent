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

(* really simple interface for now *)

type 'a cont = ('a, unit) continuation

effect Fork : (unit -> unit) -> unit
effect Yield : unit
effect Suspend : ('a cont -> unit) -> 'a
effect Resume : ('a cont * 'a) -> unit
effect GetTid : int

let fork f = perform (Fork f)
let yield () = perform Yield
let suspend f = perform (Suspend f)
let resume t v = perform (Resume (t, v))
let get_tid () = perform GetTid

open CAS.Sugar

let nb_domain = 2

let nb_idle = ref 0

(* really naive : one shared queue *)
let queue = HW_MSQueue.create ()

let fresh_tid () = Oo.id (object end)

let enqueue c = HW_MSQueue.push c queue

let rec dequeue () =
  (* what to do when the queue is empty? *)
  (* TODO: count idle domain etc...      *)
  (* Right now just retrying.            *)
  let b = Backoff.create () in
  let rec loop () = match HW_MSQueue.pop queue with
    | Some k -> continue k ()
    | None -> CAS.incr nb_idle ; (* not the most efficient... *)
              if !nb_idle = nb_domain
              then Printf.printf "= Domain %d terminated  =\n%!"
                                  (Domain.self ())
              else ( Backoff.once b ;
                     CAS.decr nb_idle ;
                     loop () )
  in loop ()
and spawn f (tid:int) = match f () with
    | () -> dequeue ()
    | effect (Fork f) k -> enqueue k ; spawn f (fresh_tid ())
    | effect Yield k -> enqueue k ; dequeue ()
    | effect (Suspend f) k -> spawn (fun () -> f k) tid
                              (* TODO: I am not sure of all the consequences *)
                              (* of that spawn...                            *)
    | effect (Resume (t, v)) k -> enqueue k ; continue t v
    | effect GetTid k -> continue k tid

let run f =
  Printf.printf   "=== Start scheduling ===\n%!" ;
  for i = 1 to nb_domain - 1  do
    Printf.printf "=    Spawn domain %d    =\n%!" i ;
    Domain.spawn (fun () -> dequeue ())
  done ;
  spawn f (fresh_tid ())
