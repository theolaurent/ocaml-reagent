
open Reagent.Sugar

type 'a t = (('a, 'a) Channel.endpoint * ('a, 'a) Channel.endpoint)

let create = Channel.create

(* TODO: it would be better with a symmetric choose? *)
let exchange e = Channel.swap (fst e) || Channel.swap (snd e)
