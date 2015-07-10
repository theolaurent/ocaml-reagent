

let main () =
  for i = 0 to 31 do
    Sched.fork (fun () -> Unix.sleep (Random.int 5) ;
                          Printf.printf "Thread [%d] slept well!\n%!"
                                        (Sched.get_tid ()))
  done ;
  Unix.sleep 1000

let _ = Sched.run main
