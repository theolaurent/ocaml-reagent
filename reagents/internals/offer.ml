
open CAS.Sugar

type 'a t = 'a Sched.cont option casref

let suspend f = perform (Sched.Suspend (fun k -> f (ref (Some k))))
let resume t a =
  let s = !t in
  match s with
  | Some k when t <!= s --> None
      -> perform (Sched.Resume (k, a))
  | _ -> ()
