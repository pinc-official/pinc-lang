module Location = Location

exception Pinc_error of string

val raise_error : Location.t -> string -> 'a
val print_error : Location.t -> string -> unit
val warn : Location.t -> string -> unit
val flush : unit -> unit
