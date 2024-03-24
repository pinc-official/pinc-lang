type t

val empty : t
val of_string : ?filename:string -> string -> t
val of_file : string -> t
val name : t -> string option
val length : t -> int
val content : t -> string
