
open CAS.Sugar
open Reaction.Sugar

type 'a status =
  | Waiting
  | Completed of 'a Reaction.t
  | WakedUp

(* The thread is suposed to be resumed only once, and when    *)
(* the message has been completed.                            *)
(* Also, all completed offers should be waken at some point.  *)
type 'a t = { state  : 'a status casref             ;
              thread : 'a Reaction.t Sched.cont }

let is_waiting o = match !(o.state) with
  | Waiting -> true
  | _       -> false

let wake o () =
  let s = !(o.state) in
  match s with
  | Completed v when (o.state <!= s --> WakedUp)
      -> ( perform ( Sched.Resume (o.thread, v)) )
  | _ -> failwith "Offer.wake: \
                   trying to wake a non-completed or already waken offer"

let complete_cas o a = (o.state <:= Waiting --> Completed a)


let suspend f =
  perform (Sched.Suspend (fun k -> f { state = ref Waiting ; thread = k }))

let try_resume o a =
  if CAS.commit (complete_cas o a) then ( wake o () ; true )
  else false

let rx_resume o a = Reaction.cas (complete_cas o a) >> Reaction.pc (wake o)
