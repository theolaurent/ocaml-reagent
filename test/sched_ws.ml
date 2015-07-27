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

module Make (S : sig val num_domains : int end) = struct

  type 'a cont = ('a, unit) continuation

  effect Fork     : (unit -> unit) -> unit
  effect Yield    : unit
  effect Suspend  : ('a cont -> unit) -> 'a
  effect Resume   : ('a cont * 'a) -> unit
  effect GetTid   : int

  let fork f      = perform (Fork f)
  let yield ()    = perform Yield
  let suspend f   = perform (Suspend f)
  let resume t v  = perform (Resume (t, v))
  let get_tid ()  = perform GetTid

  effect ForkOn     : (unit -> unit) * int -> unit
  effect NumDomains : int

  let fork_on f dom_id = perform (ForkOn (f, dom_id))
  let num_domains () = perform NumDomains

  open CAS.Sugar

  let num_idle = ref 0

  let sq = Array.init S.num_domains (fun _ -> HW_MSQueue.create ())

  let fresh_tid () = Oo.id (object end)

  let enqueue_wid c dom_id = HW_MSQueue.push (Array.get sq dom_id ) c

  let rec dequeue_wid dom_id =
    let b = Backoff.create ~max:16 () in
    let queue = Array.get sq dom_id in
    let rec loop () = match HW_MSQueue.pop queue with
      | Some k -> continue k ()
      | None ->
          CAS.incr num_idle ; (* not the most efficient... *)
          if !num_idle = S.num_domains
          then ()
          else ( Backoff.once b ;
                 CAS.decr num_idle ;
                 loop () )
    in loop ()
  and dequeue () = dequeue_wid (Domain.self ())
  and enqueue k = enqueue_wid k (Domain.self ())
  and spawn f (tid:int) = match f () with
      | () -> dequeue ()
      | effect (Fork f) k -> enqueue k ; spawn f (fresh_tid ())
      | effect Yield k -> enqueue k ; dequeue ()
      | effect (Suspend f) k -> spawn (fun () -> f k) tid
                                (* TODO: I am not sure of all the consequences *)
                                (* of that spawn...                            *)
      | effect (Resume (t, v)) k -> enqueue k ; continue t v
      | effect GetTid k -> continue k tid
      | effect NumDomains k -> continue k (S.num_domains)
      | effect (ForkOn (f, dom_id)) k ->
          (enqueue_wid k dom_id; spawn f (fresh_tid ()))

  let run_with f num_domains =
    for i = 1 to num_domains - 1  do
      Domain.spawn (fun () -> dequeue ())
    done ;
    spawn f (fresh_tid ())

  let run f = run_with f S.num_domains

end
