
module type S = sig
  type ('a, 'b) reagent

  type ('a, 'b) endpoint
  val create : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
  val swap : ('a, 'b) endpoint -> ('a, 'b) reagent
end

module Make (Sched : Scheduler.S) : S with type ('a, 'b) reagent
                                              = ('a, 'b) Reagent.Make(Sched).t
                                   = struct
  module Reagent = Reagent.Make (Sched)
  module MSQueue = MSQueue.Make (Sched)
  type ('a, 'b) reagent = ('a, 'b) Reagent.t

  open Reagent.Sugar

  (* TODO: what is the expected behaviour when posting on both *)
  (* sides of a channel?                                       *)

  type ('a, 'b) endpoint = { outgoing : ('a, 'b) Reagent.message MSQueue.t ;
                             incoming : ('b, 'a) Reagent.message MSQueue.t }

  let flip_endpoint e = { outgoing = e.incoming ;
                          incoming = e.outgoing }

  let create () =
    let e1 = { outgoing = MSQueue.create () ;
               incoming = MSQueue.create () } in
    (e1, flip_endpoint e1)


  let swap e =
    let () = Reagent.run
               (MSQueue.pop_until e.incoming
                                  Reagent.is_message_available) () in
    let rec all_messages c =
      match MSQueue.next c with
      | None -> Reagent.never
      | Some (m, nc) -> (* use Reagent.computed to get lazyness! *)
         Reagent.computed (fun a -> Reagent.constant a >>> (    Reagent.answer m
                                                            >+> all_messages nc  ))
    in
    let push_message m = Reagent.run (MSQueue.push e.outgoing) m in
    all_messages (MSQueue.snapshot e.incoming) >+> Reagent.send push_message

end
