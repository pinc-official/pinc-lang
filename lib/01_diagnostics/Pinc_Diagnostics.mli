module Location = Location

val error : Location.t -> string -> 'a
val warn : Location.t -> string -> unit
