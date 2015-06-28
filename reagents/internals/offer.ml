

open CAS.Sugar

type ('a, 'b) status =
  | Waiting of 'a
  | Answered of 'b
  | WakedUp

(* The thread is suposed to be resumed only once, and when   *)
(* the offer has been completed.                             *)
(* Also, all completed offers should be waken at some point. *)
type ('a, 'b) t = { state : ('a, 'b) status casref ;
                          thread : 'b Sched.cont }

let block_and_send (a:'a) (f:('a, 'b) t -> unit) : 'b =
  perform (Sched.Suspend (fun k -> f { state  = ref (Waiting a) ;
                                       thread = k }))

(* answering does not mean waken up *)
(* Hmmm... not in a reaction... Well this is offers, not channels !*)
let try_answer m b =
  let s = !(m.state) in
  match s with
  | Waiting a when m.state <!= s --> Answered b
      -> Some a
  | _ -> None

let wake k () =
  let s = !(k.state) in
  match s with
  | Answered v when k.state <!= s --> WakedUp
      -> perform (Sched.Resume (k.thread, v))
  | _ -> failwith "Offer.wake: trying to wake a non-answered offer"
