
open Reagent.Sugar

type 'a t = (('a, 'a) Channel.endpoint * ('a, 'a) Channel.endpoint)

let create = Channel.create

let exchange e = Channel.swap (fst e) || Channel.swap (snd e)
