
(* really simple interface for now *)

type 'a cont = C of ('a, unit) continuation * int

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
    | Some (C (k, i)) -> spawn (fun () -> continue k ()) i
    | None -> if !nb_idle + 1 = nb_domain then ()
              else ( CAS.incr nb_idle ; (* not the most efficient... *)
                     Backoff.once b ;
                     CAS.decr nb_idle ;
                     loop () )
  in loop ()
and spawn f tid = match f () with
    | () -> dequeue ()
    | effect (Fork f) k -> enqueue (C (k, tid)) ; spawn f (fresh_tid ())
    | effect Yield k -> enqueue (C (k, tid)) ; dequeue ()
    | effect (Suspend f) k -> spawn (fun () -> f (C (k, tid))) tid
    | effect (Resume ((C (t, nid)), v)) k ->
        enqueue (C (k, tid)) ; spawn (fun () -> continue t v) nid
    | effect GetTid k -> spawn (fun () -> continue k tid) tid

let run f =
  Printf.printf   "=== Start scheduling ===\n%!" ;
  for i = 1 to nb_domain - 1  do
    Printf.printf "=    Spawn domain %d    =\n%!" i ;
    Domain.spawn (fun () -> dequeue ())
  done ;
  spawn f (fresh_tid ())