
(* it is assumed to be private (no CAS) *)
(* TODO: use something else than int, to prevent overflows? *)
type t = int ref

let _ = Random.self_init ()

let create () = ref 1

let once r =
  let t = Random.int (!r) in
  r := 2 * !r ;
  if t = 0 then ()
  else ignore (Unix.select [] [] [] (0.001 *. (float_of_int t)))
