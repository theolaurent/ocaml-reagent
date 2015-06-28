
open CAS.Sugar

type 'a status =
  | Waiting
  | Completed of 'a
  | WakedUp

(* The thread is suposed to be resumed only once, and when *)
(* the offer has been completed.                           *)
(* Also, all completed offer should be waken at some point *)
type 'a t = { state : 'a status casref ; thread : 'a Sched.cont }

let make k = { state = ref Waiting ; thread = k }

let wake o () =
  let s = !(o.state) in
  match s with
  | Completed v when o.state <!= s --> WakedUp
      -> perform (Sched.Resume (o.thread, v))
  | _ -> assert false

(* completion does not wake up yet *)
let try_complete o a = o.state <!= Waiting --> Completed a

let rx_with_completion o rx a =
  Reaction.add_cas rx o.state ~expect:Waiting ~update:(Completed a)

(* consume and continue have been moved to reagent.ml to avoid circular dependencies *)
