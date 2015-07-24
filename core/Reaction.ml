(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type 'a t = {
    result   : 'a                  ;
    cas_list : CAS.t list          ;
    pc_list  : (unit -> unit) list ;
  }
(* TODO: Hmm not efficient... Sets ? Don't care. Tiny lists.  *)

let cas c = { cas_list = [ c ] ; pc_list  = [] ; result = () }
let pc  f = { pc_list  = [ f ] ; cas_list = [] ; result = () }

let count_cas r = List.length r.cas_list

(* TODO: optim with a list of ref ids? *)
let has_cas_on rx r = List.exists (fun cas -> CAS.is_on_ref cas r) rx.cas_list

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
  let rx_return = return
  let rx_value = get_value
  let ( >>= ) = bind
  let ( >> ) x y = bind x (fun _ -> y)
  let ( !! ) = try_commit
end
