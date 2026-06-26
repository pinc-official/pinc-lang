open Pinc_Types
open Pinc_Bytecode
module Stack = Vm_stack

exception TODO

type t = {
  bytecode : Bytecode.t;
  stack : Value.t Stack.t;
  mutable globals : Value.t Int32.Map.t;
}

let stack_size = 2048

let make (bytecode : Bytecode.t) =
  {
    bytecode;
    stack = Stack.make ~size:stack_size ~default_value:Value.Null;
    globals = Int32.Map.empty;
  }
;;

let rec execute_binary_operation t op =
  let r = Stack.pop t.stack in
  let l = Stack.pop t.stack in
  let result =
    match op with
    | Operators.Binary.PLUS -> execute_binary_add l r
    | Operators.Binary.MINUS -> execute_binary_sub l r
    | Operators.Binary.TIMES -> execute_binary_times l r
    | Operators.Binary.DIV -> execute_binary_div l r
    | Operators.Binary.POW -> execute_binary_pow l r
    | Operators.Binary.MODULO -> execute_binary_mod l r
    | Pinc_Types.Operators.Binary.EQUAL -> execute_binary_equal l r
    | Pinc_Types.Operators.Binary.NOT_EQUAL -> execute_binary_not_equal l r
    | Pinc_Types.Operators.Binary.GREATER -> execute_binary_greater l r
    | Pinc_Types.Operators.Binary.GREATER_EQUAL -> execute_binary_greater_equal l r
    | Pinc_Types.Operators.Binary.LESS -> execute_binary_less l r
    | Pinc_Types.Operators.Binary.LESS_EQUAL -> execute_binary_less_equal l r
    | Pinc_Types.Operators.Binary.AND -> execute_binary_and l r
    | Pinc_Types.Operators.Binary.OR -> execute_binary_or l r
    | Pinc_Types.Operators.Binary.CONCAT -> execute_binary_concat l r
    | Pinc_Types.Operators.Binary.DOT_ACCESS -> execute_binary_dot_access l r
    | Pinc_Types.Operators.Binary.BRACKET_ACCESS -> execute_binary_bracket_access l r
    | Pinc_Types.Operators.Binary.FUNCTION_CALL -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.PIPE -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.ARRAY_ADD -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.MERGE -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.RANGE -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.INCLUSIVE_RANGE -> raise_notrace TODO
  in
  Stack.push t.stack result

and execute_binary_add l r =
  match (l, r) with
  | Value.Int x, Value.Int y -> Value.Int (x + y)
  | Value.Float x, Value.Int y -> Value.Float (x +. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x +. y)
  | Value.Float x, Value.Float y -> Value.Float (x +. y)
  | (Value.Int _ | Value.Float _), _ | _, (Value.Int _ | Value.Float _) | _ ->
      raise_notrace (Invalid_argument "Trying to add non numeric values.")

and execute_binary_sub l r =
  match (l, r) with
  | Value.Int x, Value.Int y -> Value.Int (x - y)
  | Value.Float x, Value.Int y -> Value.Float (x -. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x -. y)
  | Value.Float x, Value.Float y -> Value.Float (x -. y)
  | (Value.Int _ | Value.Float _), _ | _, (Value.Int _ | Value.Float _) | _ ->
      raise_notrace (Invalid_argument "Trying to subtract non numeric values.")

and execute_binary_times l r =
  match (l, r) with
  | Value.Int x, Value.Int y -> Value.Int (x * y)
  | Value.Float x, Value.Int y -> Value.Float (x *. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x *. y)
  | Value.Float x, Value.Float y -> Value.Float (x *. y)
  | (Value.Int _ | Value.Float _), _ | _, (Value.Int _ | Value.Float _) | _ ->
      raise_notrace (Invalid_argument "Trying to multiply non numeric values.")

and execute_binary_div l r =
  match (l, r) with
  | Value.Int _, Value.Int 0
  | Value.Int _, Value.Float 0.
  | Value.Float _, Value.Float 0.
  | Value.Float _, Value.Int 0 -> raise_notrace (Invalid_argument "Division by 0.")
  | Value.Int x, Value.Int y -> Value.Float (float_of_int x /. float_of_int y)
  | Value.Float x, Value.Int y -> Value.Float (x /. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x /. y)
  | Value.Float x, Value.Float y -> Value.Float (x /. y)
  | (Value.Int _ | Value.Float _), _ | _, (Value.Int _ | Value.Float _) | _ ->
      raise_notrace (Invalid_argument "Trying to multiply non numeric values.")

and execute_binary_mod l r =
  match (l, r) with
  | Value.Int _, Value.Int 0
  | Value.Int _, Value.Float 0.
  | Value.Float _, Value.Float 0.
  | Value.Float _, Value.Int 0 -> Value.Int 0
  | Value.Int a, Value.Int b -> Value.Int (a mod b)
  | Value.Float a, Value.Float b -> Value.Float (a -. (a /. b *. b))
  | Value.Float a, Value.Int 1 -> Value.Float (fst (Float.modf a))
  | Value.Float a, Value.Int b ->
      let b = float_of_int b in
      Value.Float (a -. (a /. b *. b))
  | Int a, Float b ->
      let a = float_of_int a in
      Value.Float (a -. (a /. b *. b))
  | (Value.Int _ | Value.Float _), _ | _, (Value.Int _ | Value.Float _) | _ ->
      raise_notrace (Invalid_argument "Trying to modulo non numeric values.")

and execute_binary_pow l r =
  match (l, r) with
  | Value.Int l, Value.Int r -> Value.Float (float_of_int l ** float_of_int r)
  | Value.Float l, Value.Float r -> Value.Float (l ** r)
  | Value.Float l, Value.Int r -> Value.Float (l ** float_of_int r)
  | Value.Int l, Value.Float r -> Value.Float (float_of_int l ** r)
  | (Value.Int _ | Value.Float _), _ | _, (Value.Int _ | Value.Float _) | _ ->
      raise_notrace (Invalid_argument "Trying to raise non numeric values.")

and execute_binary_concat l r =
  let buf = Buffer.create 32 in
  let () =
    match (l, r) with
    | Value.String a, Value.String b ->
        Buffer.add_string buf a;
        Buffer.add_string buf b
    (* | Value.String a, Value.Char b ->
        Buffer.add_string buf a;
        Buffer.add_utf_8_uchar buf b
    | Value.Char a, Value.String b ->
        Buffer.add_utf_8_uchar buf a;
        Buffer.add_string buf b
    | Value.Char a, Value.Char b ->
        Buffer.add_utf_8_uchar buf a;
        Buffer.add_utf_8_uchar buf b *)
    | _ -> raise_notrace (Invalid_argument "Trying to concat non string literals.")
  in
  Value.String (Buffer.contents buf)

and execute_binary_dot_access l r =
  match (l, r) with
  | Record a, String b -> a |> StringMap.find_opt b |> Option.value ~default:Value.Null
  | Null, _ -> Value.Null
  | _ ->
      raise_notrace
      @@ Invalid_argument
           ("Trying to access a property on a non record value: " ^ Value.to_string l)

and execute_binary_bracket_access l r =
  match (l, r) with
  | Value.Array a, Value.Int b -> (
      try Array.get a b with Invalid_argument _ -> Value.Null)
  (* | Value.String a, Value.Int b -> (
      try
        let chr =
          a
          |> Pinc_Core.Utf8String.of_string_exn
          |> Pinc_Core.Utf8String.to_list
          |> Fun.flip List.nth b
        in
        Value.Char chr
      with Failure _ | Invalid_argument _ -> Value.Null) *)
  | Record a, String b -> a |> StringMap.find_opt b |> Option.value ~default:Value.Null
  | Null, _ -> Value.Null
  | Array _, _ ->
      raise_notrace @@ Invalid_argument "Cannot access array with a non integer value."
  | Record _, _ ->
      raise_notrace @@ Invalid_argument "Cannot access record with a non string value."
  | _ ->
      raise_notrace
      @@ Invalid_argument
           ("Trying to access a property on a non record or array value: "
           ^ Value.to_string l)

and execute_binary_equal l r =
  if Value.equal l r then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_not_equal l r =
  if not @@ Value.equal l r then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_greater l r =
  if Value.compare l r > 0 then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_greater_equal l r =
  if Value.compare l r >= 0 then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_less l r =
  if Value.compare l r < 0 then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_less_equal l r =
  if Value.compare l r <= 0 then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_and l r =
  if Value.is_true l && Value.is_true r then
    Value.constant_true
  else
    Value.constant_false

and execute_binary_or l r =
  if Value.is_true l || Value.is_true r then
    Value.constant_true
  else
    Value.constant_false
;;

let rec execute_unary_operation t op =
  let r = Stack.pop t.stack in
  let result =
    match op with
    | Operators.Unary.MINUS -> execute_unary_minus r
    | Operators.Unary.NOT -> execute_unary_not r
  in
  Stack.push t.stack result

and execute_unary_minus r =
  match r with
  | Value.Int i -> Value.Int (Int.neg i)
  | Float f -> Value.Float (Float.neg f)
  | _ ->
      raise_notrace
        (Invalid_argument
           "Invalid usage of unary `-` operator. You are only able to negate integers or \
            floats.")

and execute_unary_not r =
  if Value.is_true r then
    Value.constant_false
  else
    Value.constant_true
;;

let run t =
  let ip = ref 0 in
  let instruction_length = Bytes.length t.bytecode.instructions in
  while !ip < instruction_length do
    let new_ip, op = Instruction.decode t.bytecode.instructions !ip in
    let () = ip := new_ip in
    let () =
      match op with
      | Instruction.I_Pop -> ignore @@ Stack.pop t.stack
      | Instruction.I_Constant addr ->
          let constant = Int32.Map.find addr t.bytecode.constants in
          Stack.push t.stack constant
      | Instruction.I_Add -> execute_binary_operation t Operators.Binary.PLUS
      | Instruction.I_Sub -> execute_binary_operation t Operators.Binary.MINUS
      | Instruction.I_Div -> execute_binary_operation t Operators.Binary.DIV
      | Instruction.I_Mul -> execute_binary_operation t Operators.Binary.TIMES
      | Instruction.I_Mod -> execute_binary_operation t Operators.Binary.MODULO
      | Instruction.I_Pow -> execute_binary_operation t Operators.Binary.POW
      | Instruction.I_True -> Stack.push t.stack Value.constant_true
      | Instruction.I_False -> Stack.push t.stack Value.constant_false
      | Instruction.I_Equal -> execute_binary_operation t Operators.Binary.EQUAL
      | Instruction.I_Not_Equal -> execute_binary_operation t Operators.Binary.NOT_EQUAL
      | Instruction.I_Greater -> execute_binary_operation t Operators.Binary.GREATER
      | Instruction.I_Greater_Equal ->
          execute_binary_operation t Operators.Binary.GREATER_EQUAL
      | Instruction.I_Less -> execute_binary_operation t Operators.Binary.LESS
      | Instruction.I_Less_Equal -> execute_binary_operation t Operators.Binary.LESS_EQUAL
      | Instruction.I_And -> execute_binary_operation t Operators.Binary.AND
      | Instruction.I_Or -> execute_binary_operation t Operators.Binary.OR
      | Instruction.I_Concat -> execute_binary_operation t Operators.Binary.CONCAT
      | Instruction.I_Index -> execute_binary_operation t Operators.Binary.BRACKET_ACCESS
      | Instruction.I_Dot_Index -> execute_binary_operation t Operators.Binary.DOT_ACCESS
      | Instruction.I_Minus -> execute_unary_operation t Operators.Unary.MINUS
      | Instruction.I_Not -> execute_unary_operation t Operators.Unary.NOT
      | Instruction.I_Jump addr -> ip := Int32.to_int addr
      | Instruction.I_Jump_If_False addr ->
          let condition = Stack.pop t.stack in
          let () =
            if not @@ Value.is_true condition then
              ip := Int32.to_int addr
          in
          ()
      | Instruction.I_Null -> Stack.push t.stack Value.Null
      | Instruction.I_Set_Global addr ->
          let value = Stack.pop t.stack in
          t.globals <- Int32.Map.add addr value t.globals
      | Instruction.I_Get_Global addr ->
          let value = Int32.Map.find addr t.globals in
          Stack.push t.stack value
      | Instruction.I_Array length ->
          let elements = Array.of_list @@ Stack.pop_n t.stack (Int32.to_int length) in
          let value = Value.Array elements in
          Stack.push t.stack value
      | Instruction.I_Record length ->
          let int_length = Int32.to_int length in
          let values = Stack.pop_n t.stack int_length in
          let keys =
            Stack.pop_n t.stack int_length
            |> List.map (function
              | Value.String s -> s
              | _ -> assert false)
          in
          let record = StringMap.of_list @@ List.combine keys values in
          let value = Value.Record record in
          Stack.push t.stack value
    in
    ()
  done;
  t
;;

let eval bytecode =
  let vm = bytecode |> make |> run in
  vm.stack |> Stack.last_popped_element |> Value.to_string
;;
