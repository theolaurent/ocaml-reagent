
open CAS.Sugar

type 'a status =
  | Waiting
  | Completed of 'a
  | WakedUp

(* The thread is suposed to be resumed only once, and when    *)
(* the message has been completed.                            *)
(* Also, all completed offers should be waken at some point.  *)
type 'a t = { state  : 'a status casref ;
              thread : 'a Sched.cont    ;
              id     : int              }

(* TODO: why not (how to?) use the thread id + a thread local variable? *)
let count = ref 0

let rec make k =
  let s = !count in
  if (count <!= s --> (s + 1)) then
    { state = ref Waiting ; thread = k ; id = s }
  else make k (* TODO : exponential wait? *)

type id = int

let id o = o.id

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
  perform (Sched.Suspend (fun k -> f (make k)))

let try_resume o a =
  if CAS.commit (complete_cas o a) then ( wake o () ; true )
  else false
