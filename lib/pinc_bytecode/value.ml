type t =
  | Null
  | Int of int
  | Float of float
  | Bool of bool
  | Char of Uchar.t
  | String of string
  | Array of t array
  | Record of t StringMap.t
  | Function of compiled_function
  | Closure of closure
  | BuiltinFunction of builtin_function

and builtin_function = {
  num_parameters : int;
  fn_index : int;
}

and compiled_function = {
  num_locals : int;
  num_parameters : int;
  instructions : Instruction.t Array.t;
}

and closure = {
  fn : compiled_function;
  free_variables : t Int32.Map.t;
}

let pp fmt = function
  | Null -> Format.fprintf fmt "<NULL>\n%!"
  | Int i -> Format.fprintf fmt "%i\n%!" i
  | Float f -> Format.fprintf fmt "%f\n%!" f
  | Bool b -> Format.fprintf fmt "%b\n%!" b
  | Char c -> Format.fprintf fmt "%x\n%!" (Uchar.to_int c)
  | String s -> Format.fprintf fmt "%S\n%!" s
  | Array _ -> Format.fprintf fmt "<ARRAY>\n%!"
  | Record _ -> Format.fprintf fmt "<RECORD>\n%!"
  | Closure _ -> Format.fprintf fmt "<CLOSURE>\n%!"
  | Function _ -> Format.fprintf fmt "<FUNCTION>\n%!"
  | BuiltinFunction _ -> Format.fprintf fmt "<BUILTIN>\n%!"
;;

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
  | Closure _ -> ""
  | Function _ -> ""
  | BuiltinFunction _ -> ""
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
  | Closure _ -> true
  | Function _ -> true
  | BuiltinFunction _ -> true
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
  | Function a, Function b -> equal_function a b
  | Closure a, Closure b ->
      Int32.Map.equal equal a.free_variables b.free_variables && equal_function a.fn b.fn
  | BuiltinFunction a, BuiltinFunction b ->
      Int.equal a.num_parameters b.num_parameters && Int.equal a.fn_index b.fn_index
  | _ -> false

and equal_function a b = a == b

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
  | Closure _, Closure _ -> 0
  | Function _, Function _ -> 0
  | BuiltinFunction _, BuiltinFunction _ -> 0
  | _ -> 0
;;

let constant_true = Bool true
let constant_false = Bool false

let rec serialize buf t =
  match t with
  | Null -> Buffer.add_int8 buf 0x00
  | Int i ->
      Buffer.add_int8 buf 0x01;
      Buffer.add_int64_be buf @@ Int64.of_int i
  | Float f ->
      Buffer.add_int8 buf 0x02;
      Buffer.add_int64_be buf @@ Int64.bits_of_float f
  | Bool true -> Buffer.add_int8 buf 0x03
  | Bool false -> Buffer.add_int8 buf 0x04
  | Char c ->
      Buffer.add_int8 buf 0x05;
      Buffer.add_utf_8_uchar buf c
  | String s ->
      let length = String.length s in
      Buffer.add_int8 buf 0x06;
      Buffer.add_int32_be buf @@ Int32.of_int length;
      Buffer.add_string buf s
  | Array a ->
      let length = Array.length a in
      Buffer.add_int8 buf 0x07;
      Buffer.add_int32_be buf @@ Int32.of_int length;
      Array.iter (serialize buf) a
  | Record r ->
      let length = StringMap.cardinal r in
      Buffer.add_int8 buf 0x08;
      Buffer.add_int32_be buf @@ Int32.of_int length;
      StringMap.iter
        (fun key value ->
          let length = String.length key in
          Buffer.add_int32_be buf @@ Int32.of_int length;
          Buffer.add_string buf key;
          serialize buf value)
        r
  | Function f -> serialize_function buf f
  | Closure c -> serialize_closure buf c
  | BuiltinFunction f -> serialize_builtin_function buf f

and serialize_function buf f =
  let num_locals = f.num_locals in
  let num_parameters = f.num_parameters in
  let instructions = f.instructions in
  Buffer.add_int8 buf 0x09;
  Buffer.add_int32_be buf @@ Int32.of_int num_locals;
  Buffer.add_int32_be buf @@ Int32.of_int num_parameters;
  Buffer.add_int32_be buf @@ Int32.of_int (Array.length instructions);
  Array.iter
    (fun instruction ->
      let serialized = Instruction.to_bytes instruction in
      Buffer.add_bytes buf serialized)
    instructions

and serialize_builtin_function buf f =
  let num_parameters = f.num_parameters in
  let fn_index = f.fn_index in
  Buffer.add_int8 buf 0x0A;
  Buffer.add_int32_be buf @@ Int32.of_int num_parameters;
  Buffer.add_int32_be buf @@ Int32.of_int fn_index

and serialize_closure buf c =
  let fn = c.fn in
  let free_variables = c.free_variables in
  let num_free_variables = Int32.Map.cardinal free_variables in
  Buffer.add_int8 buf 0x0B;
  Buffer.add_int32_be buf @@ Int32.of_int num_free_variables;
  Int32.Map.iter
    (fun key value ->
      Buffer.add_int32_be buf key;
      serialize buf value)
    free_variables;
  serialize_function buf fn
;;

let rec deserialize bytes offset =
  let tag = Bytes.get_int8 bytes !offset in
  offset := !offset + 1;
  match tag with
  | 0x00 -> Null
  | 0x01 ->
      let i = Int64.to_int @@ Bytes.get_int64_be bytes !offset in
      offset := !offset + 8;
      Int i
  | 0x02 ->
      let f = Int64.float_of_bits @@ Bytes.get_int64_be bytes !offset in
      offset := !offset + 8;
      Float f
  | 0x03 -> Bool true
  | 0x04 -> Bool false
  | 0x05 ->
      let c = Uchar.utf_decode_uchar @@ Bytes.get_utf_8_uchar bytes !offset in
      offset := !offset + Uchar.utf_8_byte_length c;
      Char c
  | 0x06 ->
      let length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
      offset := !offset + 4;
      let s = Bytes.sub_string bytes !offset length in
      offset := !offset + length;
      String s
  | 0x07 ->
      let length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
      offset := !offset + 4;
      let a = Array.init length (fun _ -> deserialize bytes offset) in
      Array a
  | 0x08 ->
      let length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
      offset := !offset + 4;
      let r =
        StringMap.of_list
        @@ List.init length (fun _ ->
            let length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
            offset := !offset + 4;
            let key = Bytes.sub_string bytes !offset length in
            offset := !offset + length;
            let value = deserialize bytes offset in
            (key, value))
      in
      Record r
  | 0x09 -> Function (deserialize_function bytes offset)
  | 0x0A -> BuiltinFunction (deserialize_builtin_function bytes offset)
  | 0x0B -> Closure (deserialize_closure bytes offset)
  | _ -> raise @@ Invalid_argument "cannot deserialize bytecode"

and deserialize_function bytes offset =
  let num_locals = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let num_parameters = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let instructions_length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let instructions =
    Array.init instructions_length (fun _ ->
        let new_offset, res = Instruction.decode bytes !offset in
        offset := new_offset;
        res)
  in
  { num_locals; num_parameters; instructions }

and deserialize_builtin_function bytes offset =
  let num_parameters = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let fn_index = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  { num_parameters; fn_index }

and deserialize_closure bytes offset =
  let num_free_variables = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let free_variables =
    Int32.Map.of_list
    @@ List.init num_free_variables (fun _ ->
        let key = Bytes.get_int32_be bytes !offset in
        offset := !offset + 4;
        let value = deserialize bytes offset in
        (key, value))
  in
  let fn = deserialize_function bytes offset in
  { free_variables; fn }
;;
