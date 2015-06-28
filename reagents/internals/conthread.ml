
open CAS.Sugar

type 'a status =
  | Waiting
  | Fulfuilled of 'a
  | WakedUp

(* The thread is suposed to be resumed only once, and when *)
(* the offer has been completed.                           *)
(* Also, all completed offer should be waken at some point *)
type 'a t = { state : 'a status casref ; thread : 'a Sched.cont }

let make k = { state = ref Waiting ; thread = k }

let suspend f = perform (Sched.Suspend (fun k -> f (make k)))

let wake k () =
  let s = !(k.state) in
  match s with
  | Fulfuilled v when k.state <!= s --> WakedUp
      -> perform (Sched.Resume (k.thread, v))
  | _ -> assert false

(* fulfilled does not mean waken up *)
let try_fulfill k a = k.state <!= Waiting --> Fulfuilled a

(* TODO : what about the Reaction.and_thread_fulfill ? *)
let try_fulfill_and_wake_ignore k a =
  if try_fulfill k a then wake k ()
  else ()
