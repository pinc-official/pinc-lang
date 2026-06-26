type t =
  | Null
  | Int of int
  | Float of float
  | Bool of bool
  | Char of Uchar.t
  | String of string
  | Array of t array
  | Record of t StringMap.t

let rec to_string = function
  | Null -> ""
  | Int i -> string_of_int i
  | Float f when Float.is_integer f -> string_of_int (int_of_float f)
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Char c ->
      let buf = Buffer.create 32 in
      c |> Buffer.add_utf_8_uchar buf;
      Buffer.contents buf
  | String s -> s
  | Array a ->
      let buf = Buffer.create 200 in
      a
      |> Array.iteri (fun index it ->
          if index <> 0 then
            Buffer.add_string buf " ";
          Buffer.add_string buf (to_string it));
      Buffer.contents buf
  | Record m ->
      let b = Buffer.create 1024 in
      let is_first = ref true in
      StringMap.iter
        (fun _key value ->
          if not @@ !is_first then
            Buffer.add_string b "\n";
          Buffer.add_string b (to_string value);
          is_first := false)
        m;
      Buffer.contents b
;;

let is_true = function
  | Null -> false
  | Bool b -> b
  | Int _ -> true
  | Float _ -> true
  | Char _ -> true
  | String s -> s <> ""
  | Array [||] -> false
  | Array _ -> true
  | Record m -> not (StringMap.is_empty m)
;;

let rec equal a b =
  match (a, b) with
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Float a, Int b -> a = float_of_int b
  | Int a, Float b -> float_of_int a = b
  | Bool a, Bool b -> a = b
  | Char a, Char b -> Uchar.equal a b
  | String a, String b -> a = b
  | Null, Null -> true
  | Array a, Array b -> Array.equal equal a b
  | Record a, Record b -> StringMap.equal equal a b
  | _ -> false
;;

let compare a b =
  match (a, b) with
  | Int a, Int b -> Int.compare a b
  | Float a, Float b -> Float.compare a b
  | Float a, Int b -> Float.compare a (float_of_int b)
  | Int a, Float b -> Float.compare (float_of_int a) b
  | Bool a, Bool b -> Bool.compare a b
  | Char a, Char b -> Uchar.compare a b
  | Char a, Int b -> Int.compare (Uchar.to_int a) b
  | Int a, Char b -> Int.compare a (Uchar.to_int b)
  | String a, String b -> String.compare a b
  | Null, Null -> 0
  | Array a, Array b -> Int.compare (Array.length a) (Array.length b)
  | Record a, Record b -> StringMap.compare compare a b
  | _ -> 0
;;

let constant_true = Bool true
let constant_false = Bool false
