
open Reagent.Sugar

(* TODO: what is the expected behaviour when posting on both *)
(* sides of a channel?                                       *)

(* TODO: multiple message answer via choose                  *)
(*       also posting an offer on the channel ??             *)

type ('a, 'b) endpoint = { outgoing : ('a, 'b) Message.t ConcurrentQueue.t ;
                           incoming : ('b, 'a) Message.t ConcurrentQueue.t }

let flip_endpoint e = { outgoing = e.incoming ;
                        incoming = e.outgoing }

let create () =
  let e1 = { outgoing = ConcurrentQueue.create () ;
             incoming = ConcurrentQueue.create () } in
  (e1, flip_endpoint e1)


let swap e =
  let () = Reagent.run (ConcurrentQueue.pop_until e.incoming
                                                  Message.is_available) () in
  let push_message m = Reagent.run (ConcurrentQueue.push e.outgoing) m in
  (  Reagent.computed (fun a ->
       let q = ConcurrentQueue.whole e.incoming in
          Reagent.constant a
       |> FQueue.fold_right (fun m res -> Message.answer m || res) q Reagent.never)
  || Message.send push_message )
