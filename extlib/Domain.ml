
(* for compatibility with multicore version tests *)

let self () = Thread.id (Thread.self ())
let spawn (f:unit -> unit) = ignore (Thread.create f ()) (* TODO: this function should NOT *)
                                                         (* be used!!                      *)
