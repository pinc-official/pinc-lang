type t =
  | Null
  | Int of int
  | Float of float
  | Bool of bool

let pp fmt = function
  | Null -> Format.fprintf fmt "<NULL>\n%!"
  | Int i -> Format.fprintf fmt "%i\n%!" i
  | Float f -> Format.fprintf fmt "%f\n%!" f
  | Bool b -> Format.fprintf fmt "%b\n%!" b
;;

let to_string = function
  | Null -> ""
  | Int i -> string_of_int i
  | Float f when Float.is_integer f -> string_of_int (int_of_float f)
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
;;

let constant_true = Bool true
let constant_false = Bool false
