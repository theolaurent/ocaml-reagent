open CAS.Sugar

(* Michael-Scott queue, handwritten *)
(* TODO: as I don't need kCAS, should I be using directly the Obj CAS? *)

type 'a node =
  | Nil
  | Next of 'a * 'a node casref

type 'a t =
  { head : 'a node casref ;
    tail : 'a node casref }

(* the first node of the list starting a head is ignored *)
(* the tail never point to Nil                           *)
let create () =
  let head = (Next (Obj.magic (), ref Nil)) in
  { head = ref head ; tail = ref head }

let pop q =
  let b = Backoff.create () in
  let rec loop () =
    let s = !(q.head) in
    let nhead = match s with
      | Nil -> failwith "HW_MSQueue.pop: broken invariant"
      | Next (_, x) -> !x
    in match nhead with
     | Nil -> None
     | Next  (v, _) when (q.head <!= s --> nhead) -> Some v
     | _ -> Backoff.once b ; loop ()
  in loop ()

let push v q =
  let rec find_tail_and_enq curr_end node =
    if curr_end <!= (Nil --> node) then ()
    else match !curr_end with
          | Nil -> failwith "HW_MSQueue.push: broken invariant."
          | Next (_, n) -> find_tail_and_enq n node
  in
  let newnode = Next (v, ref Nil) in
  match !(q.tail) with
  | Nil         -> failwith "HW_MSQueue.push: broken invariant."
  | Next (_, n) -> find_tail_and_enq n newnode ;
                   (* try to update tail, but ignore when fail *)
                   ignore (q.tail <!= !(q.tail) --> newnode)
