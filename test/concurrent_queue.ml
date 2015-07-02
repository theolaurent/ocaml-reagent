
module Q = ConcurrentQueue
(* TODO: write real tests! *)
let () =
  let q = Q.create () in
  assert (Q.try_pop q = None) ;
  assert (Q.try_pop q = None) ;
  Q.push 1 q ;
  Q.push 2 q ;
  assert (Q.try_pop q = Some 1) ;
  Q.push 3 q ;
  assert (Q.try_pop q = Some 2) ;
  assert (Q.try_pop q = Some 3) ;
  assert (Q.try_pop q = None) ;
  Q.push 4 q ;
  Q.push 5 q ;
  assert (Q.try_pop q = Some 4) ;
  assert (Q.try_pop q = Some 5) ;
  assert (Q.try_pop q = None) ;
  Printf.printf "OK\n" ;
