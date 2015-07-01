
type t = {
    cas_list : CAS.t list ;
    pc_list  : (unit -> unit) list ;
  }

let inert = { cas_list = [] ;
              pc_list  = [] }

let cas c = { cas_list = [ c ] ; pc_list  = [] }
let pc  f = { pc_list  = [ f ] ; cas_list = [] }

let combine r1 r2 = { cas_list = r1.cas_list @ r2.cas_list ;
                      pc_list  = r1.pc_list  @ r2.pc_list  }

let c_offer o a = combine (cas (Offer.complete_cas o a))
                           (pc (Offer.wake o))

(* TODO: Hmm not efficient... Sets ? Don't care. Tiny lists.  *)

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
  let ( ++ ) = combine
  let ( !! ) = try_commit
end
