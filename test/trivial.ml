
module ReagentLib = ReagentLib.Make (Sched)
open ReagentLib


let main () =
  for i = 0 to 31 do
    Sched.fork (fun () -> Unix.sleep (Random.int 5) ;
                          Printf.printf "Domain [%d]: Thread [%d] slept well!\n%!"
                                        (Domain.self ()) (Sched.get_tid ()))
  done

let _ = Sched.run main
