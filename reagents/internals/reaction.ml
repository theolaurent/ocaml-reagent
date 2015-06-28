
open CAS.Sugar

type t = {
    cas_list : CAS.abstract_t list ;
    pc_list  : (unit -> unit) list ;
  }

let inert = { cas_list = [] ;
              pc_list  = [] }

let add_cas rx r ~expect ~update = { rx with cas_list = (r <:= expect --> update) :: rx.cas_list }
let add_pc rx f = { rx with pc_list = f :: rx.pc_list }

let combine r1 r2 = { cas_list = r1.cas_list @ r2.cas_list ;
                      pc_list  = r1.pc_list  @ r2.pc_list  }
                 (* TODO: Hmm not efficient... LazyLists ? *)


(* TODO: is it useful ? ; not for the "choose" I think ... *)
let add_thread_fulfill rx k a =
  add_pc (add_cas rx k.Conthread.state
                  ~expect:Conthread.Waiting
                  ~update:(Conthread.Fulfuilled a))
                  (Conthread.wake k)

let try_commit r =
  let success = match r.cas_list with
    | [] -> true
    | [cas] -> CAS.commit cas
    | l -> CAS.kCAS l
  in
  let () = if success then
             List.iter (fun f -> f ()) r.pc_list
  in success
