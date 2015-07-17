(* first list for out, second for in *)
type 'a t = Q of 'a list * 'a list

exception Empty

let empty = Q ([], [])

let push elt (Q (o, i)) = Q (o, elt::i)

let rec pop q = match q with
  | Q ([], []) -> raise Empty
  | Q (e::o, i) -> (e, (Q (o, i)))
  | Q ([], i) -> pop (Q (List.rev i, []))

(* this is fold_left/right considering push is analog to cons *)
let fold_left f n (Q (o, i)) =
  List.fold_right (fun x n -> f n x) o (List.fold_left f n i)
let fold_right f (Q (o, i)) n =
  List.fold_right f i (List.fold_left (fun n x -> f x n) n o)

let map f (Q (o, i)) = (Q (List.map f o, List.map f i))
