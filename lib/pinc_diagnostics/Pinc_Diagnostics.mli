module Position = Pinc_Position

val error : start_pos:Position.t -> end_pos:Position.t -> string -> 'a
val warn : start_pos:Position.t -> end_pos:Position.t -> string -> unit
