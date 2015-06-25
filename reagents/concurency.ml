
module type SCHED = sig
  type -'a cont
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
    let fulfill o = match !o with
      | None -> raise AlreadyFulfilledOffer
      | Some (x, k) -> ( o := None ; (x, k) )
    let is_fulfilled o = match !o with
      | None -> false
      | Some _ -> true
    let rec clean_queue q =
      match !(Queue.top q) with
      | Some _ -> ()
      | None -> ignore (Queue.pop q) ; clean_queue q
  end

  module Events = struct

    type (-'a, +'b) t = {
        tryDo : 'a -> 'b option  ;
        (* block is supposed to be called only when tryDo returns None *)
        block : 'a -> 'b Sc.cont -> unit ;
      }

    let choose x y =
      let tryDo v = match x.tryDo v with
        | Some r -> Some r
        | None -> y.tryDo v
      in
      let block v k = failwith "unimplemented"
      in { tryDo ; block }
  end

  module Mvar : sig
    type 'a mvar
    val new_mvar       : 'a -> 'a mvar
    val new_empty_mvar : unit -> 'a mvar
    val put_mvar_evt   : 'a mvar -> ('a, unit) Events.t
    (* val take_mvar_evt  : 'a mvar -> (unit, 'a) Events.t *)
  end = struct

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
                              let ((), k) = Offer.fulfill o in
                              resume k v )
      in
      let block v = match !mv with
        | Empty _ -> assert false
        | Full (_, q) -> (fun k -> Queue.push (Offer.make v k) q)
      in { Events.tryDo = tryDo ; Events.block = block }

    let take_mvar_evt mv =
      let tryDo () = match !mv with
        | Empty _ -> None
        | Full (v, q) -> Offer.clean_queue q ;
                         ( if Queue.is_empty q then
                             mv := Empty (Queue.create ())
                           else
                             let o = Queue.pop q in
                             let (v', k) = Offer.fulfill o in
                             mv := Full (v', q) ;
                             resume k ()
                         ) ;
                         Some v
      in
      let block () = match !mv with
        | Full _ -> assert false
        | Empty q -> (fun k -> Queue.push (Offer.make () k) q)
      in { Events.tryDo = tryDo ; Events.block = block }
  end
end
