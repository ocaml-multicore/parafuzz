type 'a cont
(** Represents a blocked computation that waits for a value of type 'a. *)

val context_switch : unit -> unit
(** [context_switch] switches to the next runnable as determined by scheduler. *)

val fork : (unit->unit) -> unit
(** [fork f] spawns a new runnable in the scheduler. *)

val suspend : ('a cont -> unit) -> 'a
(** [suspend f] applies [f] to the current continuation, and suspends the
 *  execution of the current runnable, and switches to the next runnable in the
 *  scheduler's queue. *)

val resume : ('a cont * 'a) -> unit
(** [resume (k,v)] prepares the suspended continuation [k] with value [v] and
 *  enqueues it to the scheduler queue. *)

val run : (unit -> unit) -> unit
(** [run f] runs [f] with the AFL controlled scheduler. *)

