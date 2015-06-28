module C = Concurrency.Make (Sched)

open C.Mvar
open C.Events
open Printf
open Sched

let mv_1 = new_empty_mvar ()
let mv_2 = new_empty_mvar ()

let get_tid () = Effects.perform Get_Tid
let fork f = Effects.perform @@ Fork f

let put_1 x = begin
    printf "[Thread %d] Before put_1: %s\n" (get_tid ()) x ;
    sync (put_mvar_evt mv_1) x ;
    printf "[Thread %d] After put_1: %s\n" (get_tid ()) x ;
  end

let get_1 () =
  let () = printf "[Thread %d] Before get_1\n" (get_tid ()) in
  let v = sync (take_mvar_evt mv_1) () in
  let () = printf "[Thread %d] After get_1: %s\n" (get_tid ()) v in
  v

let put_2 x = begin
    printf "[Thread %d] Before put_2: %s\n" (get_tid ()) x ;
    sync (put_mvar_evt mv_2) x ;
    printf "[Thread %d] After put_2: %s\n" (get_tid ()) x ;
  end

let get_2 () =
  let () = printf "[Thread %d] Before get_2\n" (get_tid ()) in
  let v = sync (take_mvar_evt mv_2) () in
  let () = printf "[Thread %d] After get_2: %s\n" (get_tid ()) v in
  v

let get_both () =
  let () = printf "[Thread %d] Before get_both\n" (get_tid ()) in
  let v = sync (choose (take_mvar_evt mv_1) (take_mvar_evt mv_2)) () in
  let () = printf "[Thread %d] After get_both: %s\n" (get_tid ()) v in
  v

let main () = begin
    fork (fun () -> put_1 "1") ;
    fork (fun () -> put_1 "2") ;
    fork (fun () -> put_1 "3") ;
    fork (fun () -> ignore (get_1 ())) ;
    fork (fun () -> ignore (get_1 ())) ;
    fork (fun () -> ignore (get_1 ())) ;

    fork (fun () -> put_1 "4") ;
    fork (fun () -> put_1 "5") ;
    fork (fun () -> put_2 "6") ;
    fork (fun () -> put_2 "7") ;

    fork (fun () -> ignore (get_both ())) ;

    (* hmm thread that are blocked don't prevent *)
    (* the program to terminate ...              *)
  end

let () = run main
