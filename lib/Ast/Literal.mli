type t =
  | Null
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Array of t Iter.t

val of_string : string -> t
val of_int : int -> t
val of_float : float -> t
val of_bool : bool -> t
val of_iter : t Iter.t -> t
val to_string : t -> string
val is_true : t -> bool
val negate : t -> bool
val is_numeric : t -> bool
val int_of_literal : t -> int option
val equal : t * t -> bool
val compare : t * t -> int
