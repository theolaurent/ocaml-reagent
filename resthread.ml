

type 'a res =
  | Uninitialized
  | Res of 'a
  | Exn of exn

type 'a t = {
    result : 'a res ref ;
    thread : Thread.t
  }


let create f a =
  let result = ref Uninitialized in
  let thread = Thread.create (fun x -> try
                                      let res = f x in result := Res res
                                    with e -> result := Exn e
                             ) a in
  { result ; thread }

let get_thread x = x.thread

let rec get_result t =
  let () = Thread.join t.thread in
  match !(t.result) with
  | Uninitialized -> assert false
  | Res x -> x
  | Exn e -> raise e
