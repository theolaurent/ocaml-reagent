
type 'a t = {
    result   : 'a                  ;
    cas_list : CAS.t list          ;
    pc_list  : (unit -> unit) list ;
    offers   : Offer.id list       ;
  }
(* TODO: Hmm not efficient... Sets ? Don't care. Tiny lists.  *)

let cas c = { cas_list = [ c ] ; pc_list  = [] ; offers = [] ; result = () }
let pc  f = { pc_list  = [ f ] ; cas_list = [] ; offers = [] ; result = () }

let completion o a =
  { cas_list = [ Offer.complete_cas o a ] ;
    pc_list  = [ Offer.wake o           ] ;
    offers   = [ Offer.id o             ] ;
    result   = ()                         }

let has_offer r o =
  List.mem (Offer.id o) r.offers

let try_commit r =
  let success = match r.cas_list with
    | [] -> true
    | [cas] -> CAS.commit cas
    | l -> CAS.kCAS l
  in
  let () = if success then
             List.iter (fun f -> f ()) r.pc_list
  in success

let get_value r = r.result

let return x = { result   = x ;
                 cas_list = [] ;
                 pc_list  = [] ;
                 offers   = [] ;
               }

let clear r = return r.result

let bind r1 f =
  let r2 = f r1.result in
  { cas_list = r1.cas_list @ r2.cas_list ;
    pc_list  = r1.pc_list  @ r2.pc_list  ;
    offers   = r1.offers   @ r2.offers   ;
    result   = r2.result                 }

let map f r = bind r (fun x -> return (f x))

module Sugar = struct
  let rx_return = return
  let rx_value = get_value
  let ( >>= ) = bind
  let ( >> ) x y = bind x (fun _ -> y)
  let ( !! ) = try_commit
end
