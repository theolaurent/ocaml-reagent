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
open Printf

type thread_id = int
type 'a cont = Cont : ('a,unit) continuation * thread_id -> 'a cont

type _ eff +=
  | Fork    : (unit -> unit) -> unit eff
  | Yield   : unit eff
  | Suspend : ('a cont -> unit) -> 'a eff
  | Resume  : 'a cont * 'a -> unit eff
  | GetTid : int eff

let fork f = perform (Fork f)
let yield () = perform Yield
let suspend f = perform (Suspend f)
let resume k v = perform (Resume (k,v))
let get_tid () = perform GetTid

let run main =
  (* Thread ID *)
  let cur_tid = ref (-1) in
  let next_tid = ref 0 in
  (* Run queue handling *)
  let run_q = Queue.create () in
  let enqueue t v tid =
    Queue.push (fun () -> (cur_tid := tid; continue t v)) run_q
  in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else Queue.pop run_q ()
  in
  let rec spawn : type a . (a -> unit) -> a -> unit =
    fun f x ->
      cur_tid := !next_tid;
      next_tid := !next_tid + 1;
      Effects.handle scheduler f x
    and scheduler =
      {return = dequeue;
      exn = raise;
      eff = fun (type a) (eff : a eff) (k : (a, unit) continuation) ->
        match eff with
        | Yield ->
            enqueue k () !cur_tid;
            dequeue ()
        | Fork f ->
            enqueue k () !cur_tid;
            spawn f ()
        | Suspend f ->
            (* f (Cont (k,!cur_tid)); *)
            (* dequeue () *)
            (* hack to get effects handled *)
            Effects.handle scheduler f (Cont (k,!cur_tid));
            dequeue ()
        | Resume(Cont (k',tid), v) ->
            enqueue k' v tid;
            continue k ()
        | GetTid -> continue k !cur_tid
        | _ -> delegate eff k}
  in
  spawn main ()
