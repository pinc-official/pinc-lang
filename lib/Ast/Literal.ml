type t =
  | Null
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Array of t Iter.t

let of_string s = String s
let of_int i = Int i
let of_float f = Float f
let of_bool b = Bool b
let of_iter i = Array i

let rec to_string = function
  | Null -> ""
  | String s -> s
  | Int i -> string_of_int i
  | Float f ->
    if Float.is_integer f
    then string_of_int (int_of_float f)
    else string_of_float f
  | Bool b -> if b then "true" else "false"
  | Array l ->
    let b = Buffer.create 1024 in
    Iter.iter (fun literal -> Buffer.add_string b (to_string literal)) l;
    Buffer.contents b
;;

let is_true = function
  | Null -> false
  | Bool b -> b
  | String s -> s |> String.trim |> String.length > 0
  | Int _ -> true
  | Float _ -> true
  | Array l -> not (Iter.is_empty l)
;;

let negate t = not (is_true t)

let is_numeric = function
  | Int _ -> true
  | Float _ -> true
  | Null | String _ | Bool _ | Array _ -> false
;;

let int_of_literal = function
  | Int i -> Some i
  | Float f -> Some (int_of_float f)
  | Null | String _ | Bool _ | Array _ -> None
;;

let equal = function
  | String a, String b -> String.equal a b
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Bool a, Bool b -> a = b
  | Array a, Array b -> Iter.to_rev_list a = Iter.to_rev_list b
  | Null, Null -> true
  | Null, (String _ | Int _ | Float _ | Bool _ | Array _)
  | Array _, (Null | String _ | Int _ | Float _ | Bool _)
  | Bool _, (Null | String _ | Int _ | Float _ | Array _)
  | Float _, (Null | String _ | Int _ | Bool _ | Array _)
  | Int _, (Null | String _ | Float _ | Bool _ | Array _)
  | String _, (Null | Int _ | Float _ | Bool _ | Array _) -> false
;;

let compare = function
  | String a, String b -> String.compare a b
  | Int a, Int b -> Int.compare a b
  | Float a, Float b -> Float.compare a b
  | Bool a, Bool b -> Bool.compare a b
  | Array a, Array b -> Int.compare (Iter.length a) (Iter.length b)
  | Null, Null -> 0
  | Null, (String _ | Int _ | Float _ | Bool _ | Array _) -> assert false
  | Array _, (Null | String _ | Int _ | Float _ | Bool _) -> assert false
  | Bool _, (Null | String _ | Int _ | Float _ | Array _) -> assert false
  | Float _, (Null | String _ | Int _ | Bool _ | Array _) -> assert false
  | Int _, (Null | String _ | Float _ | Bool _ | Array _) -> assert false
  | String _, (Null | Int _ | Float _ | Bool _ | Array _) -> assert false
;;