
module ReagentLib = ReagentLib.Make (Sched)
open ReagentLib

open Printf
open Reagent.Sugar
open Sched

let id_str () = sprintf "%d:%d" (Domain.self ()) (Sched.get_tid ())

let main () =
  printf "[%s] starting main\n" (id_str ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let (ep1,ep2) = Channel.create () in
  Sched.fork (fun () ->
    printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep1 >>> Channel.swap ep1) 0);
  Sched.fork (fun () -> printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep2) 1);
  printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep2) 2;

  (* Test 2 *)
  Sched.yield ();
  Unix.sleep (1);
  printf "**** Test 2 ****\n%!";
  let (ep1,ep2) = Channel.create () in
  Sched.fork (fun () ->
    printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep1 >+> Channel.swap ep2) 0);
  printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep2) 1;

  (* Test 3 *)
  Sched.yield ();
  Unix.sleep (1);
  printf "**** Test 3 ****\n%!";
  let (ep1,ep2) = Channel.create () in
  Sched.fork (fun () ->
    printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep1 >>> Channel.swap ep1) 0);
  printf "will fail! Reagents are not as powerful as communicating transactions!\n%!";
  printf "[%s] %d\n%!" (id_str ()) @@ Reagent.run (Channel.swap ep2 >>> Channel.swap ep2) 1;
  printf "should not see this!\n";
  ()

(* remark: Reagent's blocking threads are completetly ignored by the   *)
(* scheduler until they are waken, so if they aren't, the program just *)
(* terminate anywya...                                                 *)

let () = Sched.run main
