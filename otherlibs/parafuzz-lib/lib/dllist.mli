(* This file is formerly part of Lwt, released under the MIT license.
 * See LICENSE.md for details, or visit
 * https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(** Mutable double-linked list of elements *)

(** A sequence is an object holding a list of elements which support
    the following operations:

    - adding an element to the left or the right in time and space O(1)
    - taking an element from the left or the right in time and space O(1)
    - removing a previously added element from a sequence in time and space O(1)
    - removing an element while the sequence is being transversed.
*)

type 'a t
  (** Type of a sequence holding values of type ['a] *)

type 'a node
  (** Type of a node holding one value of type ['a] in a sequence *)

(** {2 Operation on nodes} *)

val get : 'a node -> 'a
  (** Returns the contents of a node *)

val set : 'a node -> 'a -> unit
  (** Change the contents of a node *)

exception Invalid_index
  (** Exception raised by [remove] *)

val remove_at_index : 'a t -> int -> 'a
  (** [remove_at_index s n] Removes 'a node at [n] index in the sequence [s]  *)

(** {2 Operations on sequence} *)

val create : unit -> 'a t
  (** [create ()] creates a new empty sequence *)

val is_empty : 'a t -> bool
  (** Returns [true] iff the given sequence is empty *)

val length : 'a t -> int
  (** Returns the number of elemenets in the given sequence in constant time. *) 

val add : 'a -> 'a t -> 'a node
  (** [add x s] appends [x] to the right of the sequence [s] *)

