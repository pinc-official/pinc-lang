open Pinc_Types
open Pinc_Bytecode
module Stack = Vm_stack
module Frame = Vm_frame

exception TODO

type t = {
  stack : Value.t Stack.t;
  mutable globals : Value.t Int32.Map.t;
  mutable frames : Frame.t list;
  constants : Value.t Int32.Map.t;
}

let stack_size = 2048

let make (bytecode : Bytecode.t) =
  let main_frame = Frame.make 0 bytecode.instructions in
  {
    constants = bytecode.constants;
    stack = Stack.make ~size:stack_size ~default_value:Value.Null;
    globals = Int32.Map.empty;
    frames = [ main_frame ];
  }
;;

let current_frame t =
  match t.frames with
  | [] -> assert false
  | hd :: _ -> hd
;;

let push_frame t frame = t.frames <- frame :: t.frames

let pop_frame t =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      t.frames <- frames;
      frame
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
    | Pinc_Types.Operators.Binary.PIPE -> assert false
    | Pinc_Types.Operators.Binary.ARRAY_ADD -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.MERGE -> raise_notrace TODO
    | Pinc_Types.Operators.Binary.RANGE -> execute_binary_range ~inclusive:false l r
    | Pinc_Types.Operators.Binary.INCLUSIVE_RANGE ->
        execute_binary_range ~inclusive:true l r
  in
  Stack.push t.stack result

and execute_binary_add l r =
  match (l, r) with
  | Value.Int x, Value.Int y -> Value.Int (x + y)
  | Value.Float x, Value.Int y -> Value.Float (x +. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x +. y)
  | Value.Float x, Value.Float y -> Value.Float (x +. y)
  | Char a, Char b -> Value.Char Uchar.(of_int (to_int a + to_int b))
  | Char a, Int b -> Value.Char Uchar.(of_int (to_int a + b))
  | Int a, Char b -> Value.Char Uchar.(of_int (a + to_int b))
  | _ -> raise_notrace (Invalid_argument "Trying to add non numeric values.")

and execute_binary_sub l r =
  match (l, r) with
  | Value.Int x, Value.Int y -> Value.Int (x - y)
  | Value.Float x, Value.Int y -> Value.Float (x -. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x -. y)
  | Value.Float x, Value.Float y -> Value.Float (x -. y)
  | Char a, Char b -> Value.Char Uchar.(of_int (to_int a - to_int b))
  | Char a, Int b -> Value.Char Uchar.(of_int (to_int a - b))
  | Int a, Char b -> Value.Char Uchar.(of_int (a - to_int b))
  | _ -> raise_notrace (Invalid_argument "Trying to subtract non numeric values.")

and execute_binary_times l r =
  match (l, r) with
  | Value.Int x, Value.Int y -> Value.Int (x * y)
  | Value.Float x, Value.Int y -> Value.Float (x *. float_of_int y)
  | Value.Int x, Value.Float y -> Value.Float (float_of_int x *. y)
  | Value.Float x, Value.Float y -> Value.Float (x *. y)
  | Char a, Char b -> Value.Char Uchar.(of_int (to_int a * to_int b))
  | Char a, Int b -> Value.Char Uchar.(of_int (to_int a * b))
  | Int a, Char b -> Value.Char Uchar.(of_int (a * to_int b))
  | _ -> raise_notrace (Invalid_argument "Trying to multiply non numeric values.")

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
    | Value.String a, Value.Char b ->
        Buffer.add_string buf a;
        Buffer.add_utf_8_uchar buf b
    | Value.Char a, Value.String b ->
        Buffer.add_utf_8_uchar buf a;
        Buffer.add_string buf b
    | Value.Char a, Value.Char b ->
        Buffer.add_utf_8_uchar buf a;
        Buffer.add_utf_8_uchar buf b
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
  | Value.String a, Value.Int b -> (
      try
        let chr =
          a
          |> Pinc_Core.Utf8String.of_string_exn
          |> Pinc_Core.Utf8String.to_list
          |> Fun.flip List.nth b
        in
        Value.Char chr
      with Failure _ | Invalid_argument _ -> Value.Null)
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

and execute_binary_range ~inclusive l r =
  let get_range from upto =
    match (from, upto) with
    | Value.Int from, Value.Int upto -> (from, upto)
    | Value.Int from, Value.Float upto when Float.is_integer upto ->
        (from, int_of_float upto)
    | Value.Float from, Value.Int upto when Float.is_integer from ->
        (int_of_float from, upto)
    | Value.Float from, Value.Float upto
      when Float.is_integer from && Float.is_integer upto ->
        (int_of_float from, int_of_float upto)
    | Int _, _ ->
        raise_notrace
        @@ Invalid_argument
             "Can't construct range. The end of your range is not of type int."
    | _, Int _ ->
        raise_notrace
        @@ Invalid_argument
             "Can't construct range. The start of your range is not of type int."
    | _, _ ->
        raise_notrace
        @@ Invalid_argument
             "Can't construct range. The start and end of your range are not of type int."
  in
  let from_int, upto_int = get_range l r in
  if from_int > upto_int then
    Value.Array [||]
  else (
    let start = from_int in
    let stop =
      if inclusive then
        upto_int + 1
      else
        upto_int
    in
    Value.Array (Array.init (stop - start) (fun i -> Value.Int (i + start))))

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

let execute_function_call t num_arguments =
  let num_arguments = Int32.to_int num_arguments in
  let fn = Stack.nth t.stack num_arguments in
  match fn with
  | Value.Function { num_parameters; _ }
    when not @@ Int.equal num_parameters num_arguments ->
      raise_notrace
      @@ Invalid_argument
           ("Trying to call a function with the wrong number of arguments. Wanted "
           ^ string_of_int num_parameters
           ^ ", got "
           ^ string_of_int num_arguments)
  | Value.Function fn ->
      let frame = Frame.make (t.stack.stack_pointer - num_arguments) fn.instructions in
      push_frame t frame;
      Stack.set_pointer t.stack (frame.base_pointer + fn.num_locals)
  | _ -> raise_notrace @@ Invalid_argument "Trying to call a non function value"
;;

let run t =
  Printexc.record_backtrace true;
  while
    (current_frame t).instruction_pointer < Bytes.length (current_frame t).instructions
  do
    let frame = current_frame t in
    let new_ip, op = Instruction.decode frame.instructions frame.instruction_pointer in
    Frame.set_instruction_pointer frame new_ip;
    let () =
      match op with
      | Instruction.I_Debug_Print_Stack ->
          Format.printf "--------- (STACK) -------\n%!";
          Stack.iter (fun value -> Format.printf "%a%!" Value.pp value) t.stack;
          Format.printf "--------- (/STACK) -------\n%!"
      | Instruction.I_Pop -> ignore @@ Stack.pop t.stack
      | Instruction.I_Constant addr ->
          let constant = Int32.Map.find addr t.constants in
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
      | Instruction.I_Range -> execute_binary_operation t Operators.Binary.RANGE
      | Instruction.I_Range_Inclusive ->
          execute_binary_operation t Operators.Binary.INCLUSIVE_RANGE
      | Instruction.I_Minus -> execute_unary_operation t Operators.Unary.MINUS
      | Instruction.I_Not -> execute_unary_operation t Operators.Unary.NOT
      | Instruction.I_Jump addr ->
          Frame.set_instruction_pointer frame @@ Int32.to_int addr
      | Instruction.I_Jump_If_False addr ->
          let condition = Stack.pop t.stack in
          let () =
            if not @@ Value.is_true condition then
              Frame.set_instruction_pointer frame @@ Int32.to_int addr
          in
          ()
      | Instruction.I_Null -> Stack.push t.stack Value.Null
      | Instruction.I_Set_Global addr ->
          let value = Stack.pop t.stack in
          t.globals <- Int32.Map.add addr value t.globals
      | Instruction.I_Get_Global addr ->
          let value = Int32.Map.find addr t.globals in
          Stack.push t.stack value
      | Instruction.I_Dynamic_Array ->
          let value = Stack.pop t.stack in
          let length =
            match value with
            | Value.Int i -> i
            | _ -> assert false
          in
          let elements = Array.of_list @@ Stack.pop_n t.stack length in
          let value = Value.Array elements in
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
      | Instruction.I_Call num_arguments -> execute_function_call t num_arguments
      | Instruction.I_Return ->
          let value = Stack.pop t.stack in
          let frame = pop_frame t in
          Stack.set_pointer t.stack frame.base_pointer;
          let () =
            (* This is the function from the I_Call instruction *)
            ignore @@ Stack.pop t.stack
          in
          Stack.push t.stack value
      | Instruction.I_Set_Local addr ->
          let value = Stack.pop t.stack in
          let address = frame.base_pointer + Int32.to_int addr in
          Stack.set t.stack address value
      | Instruction.I_Get_Local addr ->
          let address = frame.base_pointer + Int32.to_int addr in
          let value = Stack.get t.stack address in
          Stack.push t.stack value
      | Instruction.I_Length ->
          let value = Stack.pop t.stack in
          let len =
            match value with
            | Value.Array a -> Array.length a
            | Value.String s -> String.length s
            | Record r -> StringMap.cardinal r
            | _ ->
                raise_notrace
                @@ Invalid_argument
                     "Trying to call length a non array, string or record value"
          in
          Stack.push t.stack (Value.Int len)
    in
    ()
  done;
  t
;;

let eval bytecode =
  let vm = bytecode |> make |> run in
  vm.stack |> Stack.last_popped_element |> Value.to_string
;;
