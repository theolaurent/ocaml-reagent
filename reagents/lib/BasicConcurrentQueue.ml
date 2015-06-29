
open CAS.Sugar

(* TODO: this could be rewritten with reagents for boostrap! *)
type 'a node = N of ('a * 'a node) option casref

type 'a t = { head : 'a node ; tail : 'a node }
(* invariant : head = None <=> tail = None          *)
(* invariant : tail = Some (x, node) => node = None *)

let create () = { head = N (ref None) ; tail = N (ref None) }

let push v q =
  let newnode = Some (v, N (ref None)) in
  let rec loop () =
    let N head = q.head in
    let N tail = q.tail in
    let s = !(tail) in
    match s with
    | None ->
       if CAS.kCAS [ (head <:= None --> newnode) ;
                     (tail <:= None --> newnode) ]
       then () else loop ()
       (* TODO : transient failures with exponential backoff *)
    | Some (_, N node) ->
       assert (!node = None) ;
       if CAS.kCAS [ (tail <:= s    --> newnode) ;
                     (node   <:= None --> newnode) ]
       then () else loop ()
  in loop ()


let rec try_pop q =
  let N head = q.head in
  let N tail = q.tail in
  let s = !(head) in
  match s with
  | None -> None
  | Some (v, N node) -> let s' = !node in
                      match s' with
                      | None   -> if CAS.kCAS [ (head <:= s --> None) ;
                                                (tail <:= s --> None) ]
                                  then Some v else try_pop q
                      | Some _ -> if (head <!= s --> s')
                                  then Some v else try_pop q

let is_empty q =
  let N head = q.head in
  let N tail = q.tail in
  match !head with
  | None -> ( assert (!tail = None) ; true )
  | _    -> false
