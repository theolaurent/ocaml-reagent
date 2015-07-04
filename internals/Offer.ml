
open CAS.Sugar

type 'a status =
  | Waiting
  | Completed of 'a
  | WakedUp

(* The thread is suposed to be resumed only once, and when    *)
(* the message has been completed.                            *)
(* Also, all completed offers should be waken at some point.  *)
type 'a t = { state  : 'a status casref ;
              thread : 'a Sched.cont    }

type id = int

let id o = ((Obj.magic o):int)
(* I use Obj.magic because I don't want offers to carry an id, *)
(* for perfomance resons. Indeed, enforcing the uniqueness     *)
(* would require the update of a CAS reference.                *)

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
