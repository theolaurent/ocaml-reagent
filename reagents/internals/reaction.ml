
open CAS.Sugar

type t = {
    cas_list : CAS.abstract_t list ;
    pc_list  : (unit -> unit) list ;
  }

let inert = { cas_list = [] ;
              pc_list  = [] }

let add_cas rx r ~expect ~update = { rx with cas_list = (r <:= expect --> update) :: rx.cas_list }
let add_abstract_cas rx cas = { rx with cas_list = cas :: rx.cas_list }
let add_pc rx f = { rx with pc_list = f :: rx.pc_list }

let combine r1 r2 = { cas_list = r1.cas_list @ r2.cas_list ;
                      pc_list  = r1.pc_list  @ r2.pc_list  }
                 (* TODO: Hmm not efficient... Sets ? Check this order thing ...  *)

let try_commit r =
  let success = match r.cas_list with
    | [] -> true
    | [cas] -> CAS.commit cas
    | l -> CAS.kCAS l
  in
  let () = if success then
             List.iter (fun f -> f ()) r.pc_list
  in success

module Sugar = struct
  type 'a reaction_build = { rx : t ; result : 'a }
  (* TODO : monadic action (>>=, etc...) *)
  let return x = { rx = inert ; result = x }
  (* is that really resonnable? *)

  let ( ++ ) = combine
  let ( !! ) = try_commit
end
