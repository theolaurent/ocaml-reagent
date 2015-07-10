

open Reagent.Sugar

let q = MSQueue.create ()

let nb_workers = 2

let (post, get) = Channel.create ()

let step = 1000

let enqueue_work i =
  for j = (i + 1) * step to (i + 2) * step - 1 do
    let x = j - step in
    Printf.printf "Domain [%d], Worker [%d]: Enqueuing %d\n%!" (Domain.self ()) i x ;
    Reagent.run (MSQueue.push q) x
  done

let sum_work i =
  let rec loop sum = match Reagent.run (MSQueue.pop q) () with
    | None -> sum
    | Some x ->
       Printf.printf "Domain [%d], Worker [%d]: Dequeuing %d\n%!" x ;
       loop (sum + x)
  in let sum = loop 0 in
     Printf.printf "Domain [%d], Worker [%d]: Posting %d\n%!" (Domain.self ()) i sum ;
     Reagent.run (Channel.swap post) sum

let main () =
  Printf.printf "Enter main\n%!" ;
  for i = 0 to nb_workers - 1 do
    Printf.printf "Forking worker %d\n%!" i ;
    Sched.fork (fun () -> enqueue_work i ; sum_work i)
  done ;
  Printf.printf "Fetching sums\n%!" ;
  let sum () =
    let a = Array.init nb_workers (fun _ -> Reagent.run (Channel.swap get) ()) in
    Array.fold_left (+) 0 a
  in
  Printf.printf "Sum: %d\n%!" (sum ())

let () = Sched.run main
