module StringMap = Map.Make (String)

type t =
  | Null
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Array of t Iter.t
  | Record of t StringMap.t

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
    if Float.is_integer f then string_of_int (int_of_float f) else string_of_float f
  | Bool b -> if b then "true" else "false"
  | Array l ->
    let b = Buffer.create 1024 in
    Iter.iter
      (fun literal ->
        Buffer.add_string b (to_string literal);
        Buffer.add_char b '\n')
      l;
    Buffer.contents b
  | Record m ->
    let b = Buffer.create 1024 in
    m
    |> StringMap.iter (fun _key literal ->
           Buffer.add_string b (to_string literal);
           Buffer.add_char b '\n');
    Buffer.contents b
;;

let is_true = function
  | Null -> false
  | Bool b -> b
  | String s -> s |> String.trim |> String.length > 0
  | Int _ -> true
  | Float _ -> true
  | Array l -> not (Iter.is_empty l)
  | Record m -> not (StringMap.is_empty m)
;;

let negate t = not (is_true t)

let is_numeric = function
  | Int _ -> true
  | Float _ -> true
  | Null | String _ | Bool _ | Array _ | Record _ -> false
;;

let int_of_literal = function
  | Int i -> Some i
  | Float f -> Some (int_of_float f)
  | Null | String _ | Bool _ | Array _ | Record _ -> None
;;

let rec equal a b =
  match a, b with
  | String a, String b -> String.equal a b
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Bool a, Bool b -> a = b
  | Array a, Array b -> Iter.to_rev_list a = Iter.to_rev_list b
  | Record a, Record b -> StringMap.equal equal a b
  | Null, Null -> true
  | Null, (String _ | Int _ | Float _ | Bool _ | Array _ | Record _)
  | Array _, (Null | String _ | Int _ | Float _ | Bool _ | Record _)
  | Bool _, (Null | String _ | Int _ | Float _ | Array _ | Record _)
  | Float _, (Null | String _ | Int _ | Bool _ | Array _ | Record _)
  | Int _, (Null | String _ | Float _ | Bool _ | Array _ | Record _)
  | String _, (Null | Int _ | Float _ | Bool _ | Array _ | Record _)
  | Record _, (Null | Int _ | Float _ | Bool _ | Array _ | String _) -> false
;;

let rec compare a b =
  match a, b with
  | String a, String b -> String.compare a b
  | Int a, Int b -> Int.compare a b
  | Float a, Float b -> Float.compare a b
  | Bool a, Bool b -> Bool.compare a b
  | Array a, Array b -> Int.compare (Iter.length a) (Iter.length b)
  | Record a, Record b -> StringMap.compare compare a b
  | Null, Null -> 0
  | Null, (String _ | Int _ | Float _ | Bool _ | Array _ | Record _) -> assert false
  | Array _, (Null | String _ | Int _ | Float _ | Bool _ | Record _) -> assert false
  | Bool _, (Null | String _ | Int _ | Float _ | Array _ | Record _) -> assert false
  | Float _, (Null | String _ | Int _ | Bool _ | Array _ | Record _) -> assert false
  | Int _, (Null | String _ | Float _ | Bool _ | Array _ | Record _) -> assert false
  | String _, (Null | Int _ | Float _ | Bool _ | Array _ | Record _) -> assert false
  | Record _, (Null | String _ | Int _ | Float _ | Bool _ | Array _) -> assert false
;;
