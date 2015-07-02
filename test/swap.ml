open Printf
open Reagent.Sugar

let get_tid () = perform Sched.Get_Tid
let fork f = perform @@ Sched.Fork f
let yield () = perform Sched.Yield

let main () =
  printf "[%d] starting main\n" (get_tid ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let (ep1,ep2) = Channel.create () in
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep1 |> Channel.swap ep1) 0);
  fork (fun () -> printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep2) 1);
  printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep2) 2;

  (* Test 2 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 2 ****\n%!";
  let (ep1,ep2) = Channel.create () in
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep1 || Channel.swap ep2) 0);
  printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep2) 1;

  (* Test 3 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 3 ****\n%!";
  let (ep1,ep2) = Channel.create () in
  fork (fun () ->
    printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep1 |> Channel.swap ep1) 0);
  printf "will fail! Reagents are not as powerful as communicating transactions!\n";
  printf "[%d] %d\n%!" (get_tid ()) @@ Reagent.run (Channel.swap ep2 |> Channel.swap ep2) 1;
  printf "should not see this!\n";
  ()

(* remark: Reagent's blocking threads are completetly ignored by the   *)
(* scheduler until they are waken, so if they aren't, the program just *)
(* terminate anywya...                                                 *)

let () = Sched.run main
