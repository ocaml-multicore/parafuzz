module type AFLQueue = sig
    val enqueue : (unit -> unit) -> unit
    val dequeue : unit -> (unit -> unit)
    val is_empty : unit -> bool
    val range   : ?min:int -> int -> int
end

type 'a cont
(** Represents a blocked computation that waits for a value of type 'a. *)

val context_switch : unit -> unit
(** [context_switch] switches to the next runnable as determined by scheduler. *)

val fork : (unit->unit) -> unit
(** [fork f] spawns a new runnable in the scheduler. *)

val suspend : (('a cont * int) -> unit) -> 'a
(** [suspend f] applies [f] to the current continuation, and suspends the
 *  execution of the current runnable, and switches to the next runnable in the
 *  scheduler's queue. *)

val resume : ('a cont * 'a * int) -> unit
(** [resume (k,v,id)] prepares the suspended continuation [k] with value [v], 
 * domain identifier [id] and enqueues it to the scheduler queue. *)

val range : ?min:int -> int -> int
(** [range ?min n] generates integers between [min] (inclusive)
 *  and [min + n] (exclusive). Default [min] value is 0.
 *  [range ?min n] will raise [Invalid_argument] for [n <= 0].
 *)

val run : (module AFLQueue) -> (unit -> unit) -> unit
(** [run m f] runs [f] with the AFL controlled scheduler [m]. *)

val get_id : unit -> int
(* Returns the ID of the current thread running *)