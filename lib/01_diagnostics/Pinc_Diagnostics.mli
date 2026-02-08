module Location = Location

exception Pinc_error

val error : Location.t -> string -> 'a
val warn : Location.t -> string -> unit
