module Crowbar = Crowbar

val run : (unit -> unit) -> unit
(** [run m f] runs [f] with the AFL controlled scheduler [m]. *)
