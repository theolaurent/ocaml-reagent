
module type SCHED = sig
  type 'a cont
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume  : 'a cont * 'a -> unit
end

module Make (Sc: SCHED) = struct

  let suspend f = perform @@ Sc.Suspend f
  let resume k x = perform @@ Sc.Resume (k, x)

  module Offer = struct
    type ('a, 'b) t = ('a * 'b Sc.cont) option ref
    exception AlreadyFulfilledOffer
    let make v k = ref (Some (v, k))
    let set_fulfilled o = match !o with
      | None -> raise AlreadyFulfilledOffer
      | Some (x, k) -> ( o := None ; (x, k) )
    let is_fulfilled o = match !o with
      | None -> false
      | Some _ -> true
    let rec clean_queue q =
      if is_fulfilled (Queue.top q)
      then ( ignore (Queue.pop q) ; clean_queue q )
  end

  module Events = struct

    (* TODO : variance annotations *)
    type ('a, 'b) t = {
        (* tryDo returns None when the reaction can't take place.     *)
        (* For the moment it is monothread non-preemptive concurency, *)
        (* so no need for transient failures                          *)
        tryDo : 'a -> 'b option  ;
        (* withOffer shall store the offer somewhere it will be found *)
        (* by another reactant.                                       *)
        (*   ('a, 'b) Offer.t -> unit   ~~~   'a -> 'b cont -> unit   *)
        (* for now, it is assumed that withOffer is only called when  *)
        (* tryDo returns None.                                        *)
        withOffer : ('a, 'b) Offer.t -> unit ;
      }

    let sync x v = match x.tryDo v with
      | Some r -> r
      | None -> suspend (fun k -> let o = Offer.make v k in
                                  x.withOffer o)

    let choose x y =
      let tryDo v = match x.tryDo v with
        | Some r -> Some r
        | None -> y.tryDo v
      in
      let withOffer o = x.withOffer o ; y.withOffer o
      in { tryDo ; withOffer }
  end

  module Mvar = struct

    type 'a mv_state =
      | Full  of 'a * (('a ,unit) Offer.t Queue.t)
      | Empty of (unit, 'a) Offer.t Queue.t

    type 'a mvar = 'a mv_state ref

    let new_empty_mvar () = ref (Empty (Queue.create ()))

    let new_mvar v = ref (Full (v, Queue.create ()))

    let put_mvar_evt mv =
      let tryDo v = match !mv with
        | Full _ -> None
        | Empty q -> Offer.clean_queue q ;
                     Some ( if Queue.is_empty q then
                              mv := Full (v, Queue.create ())
                            else
                              let o = Queue.pop q in
                              let ((), k) = Offer.set_fulfilled o in
                              resume k v )
      in
      let withOffer o = match !mv with
        | Empty _ -> assert false (* due to assumption that tryDo retuned None *)
        | Full (_, q) -> Queue.push o q
      in { Events.tryDo = tryDo ; Events.withOffer = withOffer }

    let take_mvar_evt mv =
      let tryDo () = match !mv with
        | Empty _ -> None
        | Full (v, q) -> Offer.clean_queue q ;
                         ( if Queue.is_empty q then
                             mv := Empty (Queue.create ())
                           else
                             let o = Queue.pop q in
                             let (v', k) = Offer.set_fulfilled o in
                             mv := Full (v', q) ;
                             resume k ()
                         ) ;
                         Some v
      in
      let withOffer o = match !mv with
        | Full _ -> assert false (* due to assumption that tryDo retuned None *)
        | Empty q -> Queue.push o q
      in { Events.tryDo = tryDo ; Events.withOffer = withOffer }
  end
end
