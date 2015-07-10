
(* really simple interface for now *)

type 'a cont = { arg    : 'a option ref ;
                 cond   : Condition.t   }

let fork f = ignore (Thread.create f ())

let yield () = Thread.yield ()

let suspend f =
  let arg = ref None in
  let cond = Condition.create () in
  f { arg ; cond } ; (* TODO: check if there is a problem when f try to resume instead of parking *)
  Condition.wait cond (Mutex.create ()) ;
  ( match !arg with
    | None -> assert false
    | Some x -> x )

let resume { arg ; cond } v =
  arg := Some v ; Condition.signal cond

let get_tid () = Thread.id (Thread.self ())

let run f = f ()
