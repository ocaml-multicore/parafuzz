
type 'a t
(* The type of the value stored in the MVar *)

val make_empty : unit -> 'a t
(* Creates an empty MVar *)

val make : 'a -> 'a t
(* Creates an MVar with the given value of type 'a *)

val get : 'a t -> 'a
(* Reads the value of an MVar, if empty,
  is put in a queue for waiting *)

val put : 'a -> 'a t -> unit
(* Writes to an MVar, if already full,
  is put in a queue for waiting *)

val try_get : 'a t -> 'a option
(* Reads the value of an MVar, if empty,
  is put in a queue for waiting *)

val try_put : 'a -> 'a t -> bool
(* Writes to an MVar, if already full,
  is put in a queue for waiting *)

val put_all : 'a -> 'a t -> unit
(* Writes to an MVar, and wakes all the waiting threads up 
  if the MVar is already full, is put in a queue for waiting *)

