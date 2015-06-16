
type 'a t = {
    result : [ `BrutalFailure | `Exn of exn | `Res of 'a ] ref ;
    thread : Thread.t
  }


let spawn f a =
  let result = ref `BrutalFailure in
  let thread = Thread.create (fun x -> try
                                      let res = f x in result := `Res res
                                    with e -> result := `Exn e
                             ) a in
  { result ; thread }

let get_thread x = x.thread

let rec get_result t =
  let () = Thread.join t.thread in
  !(t.result)
