type t =
  | Null
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Array of t array

let pp fmt = function
  | Null -> Format.fprintf fmt "<NULL>\n%!"
  | Int i -> Format.fprintf fmt "%i\n%!" i
  | Float f -> Format.fprintf fmt "%f\n%!" f
  | Bool b -> Format.fprintf fmt "%b\n%!" b
  | String s -> Format.fprintf fmt "%S\n%!" s
  | Array _ -> Format.fprintf fmt "<ARRAY>\n%!"
;;

let rec to_string = function
  | Null -> ""
  | Int i -> string_of_int i
  | Float f when Float.is_integer f -> string_of_int (int_of_float f)
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> s
  | Array a ->
      let buf = Buffer.create 200 in
      a
      |> Array.iteri (fun index it ->
          if index <> 0 then
            Buffer.add_string buf " ";
          Buffer.add_string buf (to_string it));
      Buffer.contents buf
;;

let is_true = function
  | Null -> false
  | Bool b -> b
  | Int _ -> true
  | Float _ -> true
  | String s -> s <> ""
  | Array [||] -> false
  | Array _ -> true
;;

let rec equal a b =
  match (a, b) with
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Float a, Int b -> a = float_of_int b
  | Int a, Float b -> float_of_int a = b
  | Bool a, Bool b -> a = b
  | String a, String b -> a = b
  | Null, Null -> true
  | Array a, Array b -> Array.equal equal a b
  | _ -> false
;;

let compare a b =
  match (a, b) with
  | Int a, Int b -> Int.compare a b
  | Float a, Float b -> Float.compare a b
  | Float a, Int b -> Float.compare a (float_of_int b)
  | Int a, Float b -> Float.compare (float_of_int a) b
  | Bool a, Bool b -> Bool.compare a b
  | String a, String b -> String.compare a b
  | Null, Null -> 0
  | Array a, Array b -> Int.compare (Array.length a) (Array.length b)
  | _ -> 0
;;

let constant_true = Bool true
let constant_false = Bool false
