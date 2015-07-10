
open CAS.Sugar
open Reaction.Sugar

type 'a status =
  | Waiting
  | Completed of 'a
  | WakedUp

(* The thread is suposed to be resumed only once, and when    *)
(* the message has been completed.                            *)
(* Also, all completed offers should be waken at some point.  *)
type 'a t_struct = { state  : 'a status casref ;
                     thread : 'a Sched.cont    }

type _ t =
  | Dummy  : unit t (* for catalysis *)
  | Actual : 'a t_struct -> 'a t

(* TODO : removable catalysits *)
let catalist = Dummy

let is_waiting (type a) (o:a t) : bool = match o with
  | Dummy -> true
  | Actual o -> begin match !(o.state) with
                      | Waiting -> true
                      | _       -> false
                end
let rx_complete (type a) (o:a t) (a:a) : unit Reaction.t =
  let wake x () =
    let s = !(x.state) in
    match s with
    | Completed v when (x.state <!= s --> WakedUp)
        -> Sched.resume x.thread v
    | _ -> raise (Invalid_argument "Offer.rx_complete: \
             trying to wake a non-completed or already waken offer")
  in
  let complete_cas x a = (x.state <:= Waiting --> Completed a)
  in match o with
  | Dummy -> rx_return ()
  | Actual x -> Reaction.cas (complete_cas x a) >> Reaction.pc (wake x)

let rx_has (type a) rx (o:a t) : bool = match o with
  | Dummy -> false
  | Actual x -> Reaction.has_cas_on rx x.state

let suspend f =
  Sched.suspend (fun k -> f (Actual { state = ref Waiting ; thread = k }))




(*

let try_resume o a =
  if CAS.commit (complete_cas o a) then ( wake o () ; true )
  else false

 *)
