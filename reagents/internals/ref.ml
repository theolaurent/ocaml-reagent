
open CAS.Sugar
open Reagent

(* hmmm... I don't use offer there ... ; TODO: think about that ...*)


let read (r:'a casref) : (unit, 'a, 'b) t =
  let tryReact (type u) (type v) (type w) ~arg:() ~rx ~(next:(u, v, w) t option) =
    match next with
    | None -> Final (Obj.magic (!r), rx)
    | Some re -> re.tryReact ~arg:(Obj.magic !r) ~rx ~next:None
  in { tryReact }


(* I follow the scala implementation, but what about a cas build on-the-fly ? *)
let cas (r:'a casref) ~(expect:'a) ~(update:'a) : (unit, unit, 'b) t =
  let tryReact (type u) ~arg:() ~rx ~(next:('a, u, 'b) t option) =
    (* TODO : optimisation with canCASImmediate, cf scala code *)
    let rx = Reaction.add_cas rx r ~expect ~update in
    match next with
    | None -> Final ((), rx) (* type problem with the unit GADTs are maybe a solution ? *)
    | Some re -> re.tryReact ~arg:() ~rx ~next:None
  in { tryReact }
