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

module ReagentLib = ReagentLib.Make (Sched_sq)
open ReagentLib

open Reagent.Sugar

let() = Sched_sq.run (fun () ->
  let c1 = Reagent.lift (fun x -> List.length x > 0) in
  let c2 = Reagent.lift (fun y -> Printf.sprintf "%B" y) in
  let c3 = Reagent.lift (fun l -> String.concat " " (List.map string_of_int l)) in
  let c4 = Reagent.lift (fun (x, y) -> "(" ^ x ^ "," ^ y ^ ")") in
  print_endline @@ Reagent.run (((Reagent.never >+> (c1 >>> c2)) >*> c3) >>> c4) [1;2;3] ;
                  )
