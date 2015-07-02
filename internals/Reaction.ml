
type 'a t = {
    result   : 'a                  ;
    cas_list : CAS.t list          ;
    pc_list  : (unit -> unit) list ;
  }
(* TODO: Hmm not efficient... Sets ? Don't care. Tiny lists.  *)

let cas c = { cas_list = [ c ] ; pc_list  = [] ; result = () }
let pc  f = { pc_list  = [ f ] ; cas_list = [] ; result = () }

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
               }

let clear r = return r.result

let bind r1 f =
  let r2 = f r1.result in
  { cas_list = r1.cas_list @ r2.cas_list ;
    pc_list  = r1.pc_list  @ r2.pc_list  ;
    result   = r2.result                 }

let map f r = bind r (fun x -> return (f x))

module Sugar = struct
  let return = return
  let get_value = get_value
  let ( >>= ) = bind
  let ( >> ) x y = bind x (fun _ -> y)
  let ( !! ) = try_commit
end
