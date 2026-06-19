type t =
  | Null
  | Int of int

let pp fmt = function
  | Null -> Format.fprintf fmt "<NULL>\n%!"
  | Int i -> Format.fprintf fmt "%i\n%!" i
;;

let to_string = function
  | Null -> ""
  | Int i -> string_of_int i
;;
