
open CAS.Sugar

type t = {
    cas_list : CAS.t list ;
    (* TODO: the offer thing? *)
    pc_list  : (unit -> unit) list ;
  }

let inert = { cas_list = [] ;
              pc_list  = [] }

(* TODO: no order problems?? *)
let add_cas rx r ~ov ~nv = { rx with cas_list = (r <:= ov --> nv) :: rx.cas_list }
let add_pc rx f = { rx with pc_list = f :: rx.pc_list }

let (++) r1 r2 = { cas_list = r1.cas_list @ r2.cas_list ;
                   pc_list  = r1.pc_list  @ r2.pc_list  }
                 (* TODO: Hmm not efficient... LazyLists ? *)


let try_commit r =
  let success = match r.cas_list with
    | [] -> true
    | [cas] -> CAS.commit cas
    | l -> CAS.kCAS l
  in
  let () = if success then
             List.iter (fun f -> f ()) r.pc_list
  in success
