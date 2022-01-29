type t =
  | Null
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Array of t list

val to_string : t -> string
val is_true : t -> bool
val negate : t -> bool
val is_numeric : t -> bool
val int_of_literal : t -> int option
val equal : t * t -> bool
val compare : t * t -> int
