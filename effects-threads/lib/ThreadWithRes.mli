
(** Threads for which we want to get a result *)
type 'a t

(** [spawn funct arg] spawn a new thread with [Thread.create] *)
val spawn : ('a -> 'b) -> 'a -> 'b t

(** [get_thread] return the thread of its argument *)
val get_thread : 'a t -> Thread.t

(** [get_result th] suspends the execution of the calling thread
    until the thread of [th] has terminated. And then return its result
    or the exception it has raised or [`BrutalFailure] if it has brutally stopped
    without more explanation *)
val get_result : 'a t -> [ `BrutalFailure | `Exn of exn | `Res of 'a ]
