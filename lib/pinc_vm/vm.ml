open Pinc_Types
open Pinc_Bytecode
module Stack = Vm_stack

type t = {
  bytecode : Bytecode.t;
  stack : Value.t Stack.t;
}

let stack_size = 2048

let make (bytecode : Bytecode.t) =
  { bytecode; stack = Stack.make ~size:stack_size ~default_value:Value.Null }
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
    | _ -> assert false
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
;;

let run t =
  let ip = ref 0 in
  let instruction_length = Bytes.length t.bytecode.instructions in
  while !ip < instruction_length do
    let new_ip, op = Instruction.decode t.bytecode.instructions !ip in
    let () =
      match op with
      | Instruction.I_Pop -> ignore @@ Stack.pop t.stack
      | Instruction.I_Constant addr ->
          let constant = UInt16.Map.find addr t.bytecode.constants in
          Stack.push t.stack constant
      | Instruction.I_Add -> execute_binary_operation t Operators.Binary.PLUS
      | Instruction.I_Sub -> execute_binary_operation t Operators.Binary.MINUS
      | Instruction.I_Div -> execute_binary_operation t Operators.Binary.DIV
      | Instruction.I_Mul -> execute_binary_operation t Operators.Binary.TIMES
      | Instruction.I_Mod -> execute_binary_operation t Operators.Binary.MODULO
      | Instruction.I_Pow -> execute_binary_operation t Operators.Binary.POW
      | Instruction.I_True -> Stack.push t.stack Value.constant_true
      | Instruction.I_False -> Stack.push t.stack Value.constant_false
    in
    ip := new_ip
  done;
  t
;;

let eval bytecode =
  let vm = bytecode |> make |> run in
  vm.stack |> Stack.last_popped_element |> Value.to_string
;;
