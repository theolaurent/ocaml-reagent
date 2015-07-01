
open CAS.Sugar
open Reaction.Sugar

type 'a t = 'a Sched.cont option casref

let suspend f =
  perform (Sched.Suspend (fun k -> f (ref (Some k))))

let try_resume t a =
  let s = !t in
  match s with
  | Some k when (t <!= s --> None)
         -> ( perform (Sched.Resume (k, a)) ; true )
  | _ -> false
