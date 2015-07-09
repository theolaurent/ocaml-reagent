
(* really simple interface for now *)

type 'a cont = C of ('a, unit) continuation * int

effect Fork : (unit -> unit) -> unit
effect Yield : unit
effect Suspend : ('a cont -> unit) -> 'a
effect Resume : ('a cont * 'a) -> unit
effect GetTid : int

let fork f =
  Printf.printf "performing Fork\n" ;
  perform (Fork f)
let yield () =
  Printf.printf "performing Yield\n" ;
  perform Yield
let suspend f =
  Printf.printf "performing Suspend\n" ;
  perform (Suspend f)
let resume t v =
  Printf.printf "performing Resume\n" ;
  perform (Resume (t, v))
let get_tid () =
  Printf.printf "performing GetTid\n" ;
  perform GetTid

open CAS.Sugar

let nb_domain = 8

(* really naive : one shared queue *)
let queue = HW_MSQueue.create ()

let fresh_tid () = Oo.id (object end)

let enqueue c = HW_MSQueue.push c queue

let rec dequeue () =
  let C (k, i) = HW_MSQueue.pop queue in
  spawn (fun () -> continue k ()) i

and spawn f tid = match f () with
    | () -> dequeue ()
    | effect (Fork f) k -> enqueue (C (k, tid)) ;
                           spawn f (fresh_tid ())
    | effect Yield k -> enqueue (C (k, tid)) ;
                        dequeue ()
    | effect (Suspend f) k -> spawn (fun () -> f (C (k, tid))) tid
    | effect (Resume ((C (t, nid)), v)) k -> enqueue (C (k, tid)) ;
                                            spawn (fun () -> continue t v) nid
    | effect GetTid k -> spawn (fun () -> continue k tid) tid

let run f =
  for i = 1 to nb_domain - 1  do
    Domain.spawn (fun () -> dequeue ())
  done ;
  spawn f (fresh_tid ())
