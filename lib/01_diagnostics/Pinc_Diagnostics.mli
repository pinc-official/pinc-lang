module Location = Location

exception Pinc_error

val raise_type_error : Location.t -> string -> 'a
val print_type_error : Location.t -> string -> unit
val raise_error : Location.t -> string -> 'a
val print_error : Location.t -> string -> unit
val warn : Location.t -> string -> unit
