open Pinc_Bytecode
module Stack = Vm_stack

exception TODO

let stack_size = 2048

type t = {
  stack : Vm_stack.t;
  resolved_functions : (t -> t) Array.t Array.t;
  globals : Value.t Array.t;
  past_frames : frame list;
  current_frame : frame;
  constants : Value.t Array.t;
}

and frame = {
  mutable instruction_pointer : int;
  base_pointer : int;
  closure : Value.closure;
}

let make_frame ~base_pointer ~closure = { base_pointer; closure; instruction_pointer = 0 }
let[@inline] frame_free_variables frame = frame.closure.free_variables
let[@inline] current_frame t = t.current_frame

let push_frame t frame =
  { t with past_frames = t.current_frame :: t.past_frames; current_frame = frame }
;;

let pop_frame t =
  match t.past_frames with
  | [] -> assert false
  | frame :: frames ->
      let current_frame = t.current_frame in
      let t' = { t with past_frames = frames; current_frame = frame } in
      (t', current_frame)
;;

let[@inline] set_instruction_pointer t i =
  t.current_frame.instruction_pointer <- i;
  t
;;

let[@inline] incr_instruction_pointer t =
  set_instruction_pointer t @@ succ t.current_frame.instruction_pointer
;;

let[@inline] call_current_instruction t =
  let frame = current_frame t in
  let fn_id = frame.closure.fn.fn_addr in
  let instructions = Array.unsafe_get t.resolved_functions fn_id in
  let fn = Array.unsafe_get instructions frame.instruction_pointer in
  fn t
;;

let[@inline] call_next_instruction t =
  let t = incr_instruction_pointer t in
  call_current_instruction t
;;

let make ~constants ~resolved_functions ~instructions =
  let resolved_functions = Array.append resolved_functions [| instructions |] in
  let main_fn_addr = Array.length resolved_functions - 1 in
  let closure =
    Value.
      {
        fn =
          {
            fn_addr = main_fn_addr;
            num_locals = 0;
            num_parameters = 0;
            instructions = [||];
          };
        free_variables = [||];
      }
  in
  let main_frame = make_frame ~base_pointer:0 ~closure in
  {
    constants;
    resolved_functions;
    stack = Stack.make ~size:stack_size;
    globals = Array.make 131_072 Value.Null;
    past_frames = [];
    current_frame = main_frame;
  }
;;

let execute_binary_add t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_int t.stack (l + r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l +. r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_float t.stack (float_of_int l +. r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l +. float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_char t.stack (l + r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_char t.stack (l + r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_char t.stack (l + r)
    | _ -> raise_notrace (Invalid_argument "Trying to add non numeric values.")
  in
  call_next_instruction t
;;

let execute_binary_sub t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_int t.stack (l - r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l -. r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_float t.stack (float_of_int l -. r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l -. float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_char t.stack (l - r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_char t.stack (l - r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_char t.stack (l - r)
    | _ -> raise_notrace (Invalid_argument "Trying to subtract non numeric values.")
  in
  call_next_instruction t
;;

let execute_binary_times t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_int t.stack (l * r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l *. r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_float t.stack (float_of_int l *. r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l *. float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_char t.stack (l * r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_char t.stack (l * r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_char t.stack (l * r)
    | _ -> raise_notrace (Invalid_argument "Trying to multiply non numeric values.")
  in
  call_next_instruction t
;;

let execute_binary_div t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        let result =
          try float_of_int l /. float_of_int r
          with Division_by_zero -> raise_notrace (Invalid_argument "Division by 0.")
        in
        Stack.push_float t.stack result
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        let result =
          try l /. float_of_int r
          with Division_by_zero -> raise_notrace (Invalid_argument "Division by 0.")
        in
        Stack.push_float t.stack result
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        let result =
          try l /. r
          with Division_by_zero -> raise_notrace (Invalid_argument "Division by 0.")
        in
        Stack.push_float t.stack result
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        let result =
          try l /. float_of_int r
          with Division_by_zero -> raise_notrace (Invalid_argument "Division by 0.")
        in
        Stack.push_float t.stack result
    | _ -> raise_notrace (Invalid_argument "Trying to multiply non numeric values.")
  in
  call_next_instruction t
;;

let execute_binary_mod t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        let result = try l mod r with Division_by_zero -> 0 in
        Stack.push_int t.stack result
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        let result = try l -. (l /. r *. r) with Division_by_zero -> 0. in
        Stack.push_float t.stack result
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        let result =
          try
            let l = float_of_int l in
            l -. (l /. r *. r)
          with Division_by_zero -> 0.
        in
        Stack.push_float t.stack result
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        let result =
          try
            if r = 1 then
              fst (Float.modf l)
            else (
              let r = float_of_int r in
              l -. (l /. r *. r))
          with Division_by_zero -> 0.
        in
        Stack.push_float t.stack result
    | _ -> raise_notrace (Invalid_argument "Trying to modulo non numeric values.")
  in
  call_next_instruction t
;;

let execute_binary_pow t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_float t.stack (float_of_int l ** float_of_int r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l ** r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_float t.stack (float_of_int l ** r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_float t.stack (l ** float_of_int r)
    | _ -> raise_notrace (Invalid_argument "Trying to raise non numeric values.")
  in
  call_next_instruction t
;;

let execute_binary_concat t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let result =
    let buf = Buffer.create 32 in
    let () =
      match (r_tag, l_tag) with
      | Tag_Char, Tag_Char ->
          let r = Uchar.unsafe_of_int @@ Stack.pop_char t.stack in
          let l = Uchar.unsafe_of_int @@ Stack.pop_char t.stack in
          Buffer.add_utf_8_uchar buf l;
          Buffer.add_utf_8_uchar buf r
      | Tag_Char, Tag_Obj ->
          let r = Uchar.unsafe_of_int @@ Stack.pop_char t.stack in
          let l =
            match Stack.pop_value t.stack with
            | Value.String s -> s
            | _ ->
                raise_notrace (Invalid_argument "Trying to concat non string literals.")
          in
          Buffer.add_string buf l;
          Buffer.add_utf_8_uchar buf r
      | Tag_Obj, Tag_Char ->
          let r =
            match Stack.pop_value t.stack with
            | Value.String s -> s
            | _ ->
                raise_notrace (Invalid_argument "Trying to concat non string literals.")
          in
          let l = Uchar.unsafe_of_int @@ Stack.pop_char t.stack in
          Buffer.add_utf_8_uchar buf l;
          Buffer.add_string buf r
      | Tag_Obj, Tag_Obj ->
          let r =
            match Stack.pop_value t.stack with
            | Value.String s -> s
            | _ ->
                raise_notrace (Invalid_argument "Trying to concat non string literals.")
          in
          let l =
            match Stack.pop_value t.stack with
            | Value.String s -> s
            | _ ->
                raise_notrace (Invalid_argument "Trying to concat non string literals.")
          in
          Buffer.add_string buf l;
          Buffer.add_string buf r
      | _ -> raise_notrace (Invalid_argument "Trying to concat non string literals.")
    in
    Value.String (Buffer.contents buf)
  in
  Stack.push_value t.stack result;
  call_next_instruction t
;;

let execute_binary_dot_access t =
  let r = Stack.pop_value t.stack in
  let l = Stack.pop_value t.stack in
  let result =
    match (l, r) with
    | Record a, String b -> a |> StringMap.find_opt b |> Option.value ~default:Value.Null
    | Null, _ -> Value.Null
    | _ ->
        raise_notrace
        @@ Invalid_argument
             ("Trying to access a property on a non record value: " ^ Value.to_string l)
  in
  Stack.push_value t.stack result;
  call_next_instruction t
;;

let execute_binary_bracket_access t =
  let r = Stack.pop_value t.stack in
  let l = Stack.pop_value t.stack in
  let result =
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
  in
  Stack.push_value t.stack result;
  call_next_instruction t
;;

let execute_binary_range ~inclusive =
 fun t ->
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        if l > r then
          Stack.push_value t.stack @@ Value.Array [||]
        else (
          let start = l in
          let stop =
            if inclusive then
              r + 1
            else
              r
          in
          Stack.push_value t.stack
          @@ Value.Array (Array.init (stop - start) (fun i -> Value.Int (i + start))))
    | Tag_Int, _ ->
        raise_notrace
        @@ Invalid_argument
             "Can't construct range. The end of your range is not of type int."
    | _, Tag_Int ->
        raise_notrace
        @@ Invalid_argument
             "Can't construct range. The start of your range is not of type int."
    | _ ->
        raise_notrace
        @@ Invalid_argument
             "Can't construct range. The start and end of your range are not of type int."
  in

  call_next_instruction t
;;

let execute_binary_equal t =
  let r = Stack.pop_value t.stack in
  let l = Stack.pop_value t.stack in
  let () = Stack.push_bool t.stack @@ Value.equal l r in
  call_next_instruction t
;;

let execute_binary_not_equal t =
  let r = Stack.pop_value t.stack in
  let l = Stack.pop_value t.stack in
  let () = Stack.push_bool t.stack @@ not @@ Value.equal l r in
  call_next_instruction t
;;

let execute_binary_greater t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l > r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l > r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (float_of_int l > r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l > float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l > r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l > r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l > r)
    | _ ->
        let r = Stack.pop_value t.stack in
        let l = Stack.pop_value t.stack in
        Stack.push_bool t.stack (Value.compare l r > 0)
  in
  call_next_instruction t
;;

let execute_binary_greater_equal t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l >= r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l >= r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (float_of_int l >= r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l >= float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l >= r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l >= r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l >= r)
    | _ ->
        let r = Stack.pop_value t.stack in
        let l = Stack.pop_value t.stack in
        Stack.push_bool t.stack (Value.compare l r >= 0)
  in
  call_next_instruction t
;;

let execute_binary_less t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l < r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l < r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (float_of_int l < r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l < float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l < r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l < r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l < r)
    | _ ->
        let r = Stack.pop_value t.stack in
        let l = Stack.pop_value t.stack in
        Stack.push_bool t.stack (Value.compare l r < 0)
  in
  call_next_instruction t
;;

let execute_binary_less_equal t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Int, Tag_Int ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l <= r)
    | Tag_Float, Tag_Float ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l <= r)
    | Tag_Float, Tag_Int ->
        let r = Stack.pop_float t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (float_of_int l <= r)
    | Tag_Int, Tag_Float ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_float t.stack in
        Stack.push_bool t.stack (l <= float_of_int r)
    | Tag_Char, Tag_Char ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l <= r)
    | Tag_Char, Tag_Int ->
        let r = Stack.pop_char t.stack in
        let l = Stack.pop_int t.stack in
        Stack.push_bool t.stack (l <= r)
    | Tag_Int, Tag_Char ->
        let r = Stack.pop_int t.stack in
        let l = Stack.pop_char t.stack in
        Stack.push_bool t.stack (l <= r)
    | _ ->
        let r = Stack.pop_value t.stack in
        let l = Stack.pop_value t.stack in
        Stack.push_bool t.stack (Value.compare l r <= 0)
  in
  call_next_instruction t
;;

let execute_binary_and t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Bool, Tag_Bool ->
        let r = Stack.pop_bool t.stack in
        let l = Stack.pop_bool t.stack in
        Stack.push_bool t.stack (l && r)
    | _ ->
        let r = Stack.pop_value t.stack in
        let l = Stack.pop_value t.stack in
        Stack.push_bool t.stack (Value.is_true l && Value.is_true r)
  in
  call_next_instruction t
;;

let execute_binary_or t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let l_tag = Stack.peek_tag t.stack 1 in
  let () =
    match (r_tag, l_tag) with
    | Tag_Bool, Tag_Bool ->
        let r = Stack.pop_bool t.stack in
        let l = Stack.pop_bool t.stack in
        Stack.push_bool t.stack (l || r)
    | _ ->
        let r = Stack.pop_value t.stack in
        let l = Stack.pop_value t.stack in
        Stack.push_bool t.stack (Value.is_true l || Value.is_true r)
  in
  call_next_instruction t
;;

let execute_unary_minus t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let () =
    match r_tag with
    | Tag_Int ->
        let r = Stack.pop_int t.stack in
        Stack.push_int t.stack (0 - r)
    | Tag_Float ->
        let r = Stack.pop_float t.stack in
        Stack.push_float t.stack (0. -. r)
    | _ ->
        raise_notrace
          (Invalid_argument
             "Invalid usage of unary `-` operator. You are only able to negate integers \
              or floats.")
  in
  call_next_instruction t
;;

let execute_unary_not t =
  let r_tag = Stack.peek_tag t.stack 0 in
  let () =
    match r_tag with
    | Tag_Null ->
        let () = Stack.drop t.stack in
        Stack.push_bool t.stack true
    | Tag_Bool ->
        let r = Stack.pop_bool t.stack in
        Stack.push_bool t.stack (not r)
    | _ ->
        let r = Stack.pop_value t.stack in
        Stack.push_bool t.stack @@ not @@ Value.is_true r
  in
  call_next_instruction t
;;

let rec execute_function_call num_arguments =
 fun t ->
  let fn = Stack.nth t.stack num_arguments in
  match fn with
  | Value.Closure { fn = { num_parameters; _ }; _ }
  | Value.BuiltinFunction { num_parameters; _ }
    when not @@ Int.equal num_parameters num_arguments ->
      raise_notrace
      @@ Invalid_argument
           ("Trying to call a function with the wrong number of arguments. Wanted "
           ^ string_of_int num_parameters
           ^ ", got "
           ^ string_of_int num_arguments)
  | Value.Closure closure -> call_closure t ~closure ~num_arguments
  | Value.BuiltinFunction { fn_index; _ } -> call_builtin t ~fn_index ~num_arguments
  | _ -> raise_notrace @@ Invalid_argument "Trying to call a non function value"

and call_builtin ~fn_index ~num_arguments t =
  let arguments = Stack.pop_n t.stack num_arguments in
  let fn = Pinc_Bytecode.Externals.get_function fn_index in
  let value = fn ~arguments in
  (* Pop the builtin function from the stack *)
  let () = Stack.drop t.stack in
  Stack.push_value t.stack value;
  call_next_instruction t

and call_closure ~closure ~num_arguments t =
  let frame = make_frame ~base_pointer:(t.stack.stack_pointer - num_arguments) ~closure in
  let t = push_frame t frame in
  Stack.set_pointer t.stack (frame.base_pointer + closure.fn.num_locals);
  call_current_instruction t
;;

let execute_length t =
  let value = Stack.pop_value t.stack in
  let len =
    match value with
    | Value.Array a -> Array.length a
    | Value.String s -> String.length s
    | Record r -> StringMap.cardinal r
    | _ ->
        raise_notrace
        @@ Invalid_argument "Trying to call length a non array, string or record value"
  in
  Stack.push_int t.stack len;
  call_next_instruction t
;;

let execute_debug_print_stack t =
  Format.printf "--------- (STACK) -------\n%!";
  Stack.iter (fun value -> Format.printf "%a%!" Value.pp value) t.stack;
  Format.printf "--------- (/STACK) -------\n%!";
  call_next_instruction t
;;

let execute_pop t =
  Stack.drop t.stack;
  call_next_instruction t
;;

let execute_constant addr =
 fun t ->
  let constant = Array.get t.constants addr in
  Stack.push_value t.stack constant;
  call_next_instruction t
;;

let execute_true t =
  Stack.push_bool t.stack true;
  call_next_instruction t
;;

let execute_false t =
  Stack.push_bool t.stack false;
  call_next_instruction t
;;

let execute_null t =
  Stack.push_null t.stack;
  call_next_instruction t
;;

let execute_jump addr =
 fun t ->
  let t = set_instruction_pointer t addr in
  call_current_instruction t
;;

let execute_jump_if_false addr =
 fun t ->
  let condition_tag = Stack.peek_tag t.stack 0 in
  let is_false =
    match condition_tag with
    | Tag_Bool -> not @@ Stack.pop_bool t.stack
    | Tag_Null ->
        Stack.drop t.stack;
        true
    | _ ->
        let condition = Stack.pop_value t.stack in
        not @@ Value.is_true condition
  in
  if is_false then (
    let t = set_instruction_pointer t addr in
    call_current_instruction t)
  else
    call_next_instruction t
;;

let execute_set_global addr =
 fun t ->
  let value = Stack.pop_value t.stack in
  Array.set t.globals addr value;
  call_next_instruction t
;;

let execute_get_global addr =
 fun t ->
  let value = Array.get t.globals addr in
  Stack.push_value t.stack value;
  call_next_instruction t
;;

let execute_get_builtin fn_index =
  let num_parameters = Pinc_Bytecode.Externals.expected_parameters fn_index in
  let value = Value.BuiltinFunction { num_parameters; fn_index } in
  fun t ->
    Stack.push_value t.stack value;
    call_next_instruction t
;;

let execute_get_free addr =
 fun t ->
  let value = Array.get (frame_free_variables (current_frame t)) addr in
  Stack.push_value t.stack value;
  call_next_instruction t
;;

let execute_set_local addr =
 fun t ->
  let frame = current_frame t in
  let addr = frame.base_pointer + addr in
  let () = Stack.move_from_top t.stack addr in
  call_next_instruction t
;;

let execute_get_local addr =
 fun t ->
  let frame = current_frame t in
  let address = frame.base_pointer + addr in
  let () = Stack.copy_to_top t.stack address in
  call_next_instruction t
;;

let execute_array t =
  let length =
    match Stack.peek_tag t.stack 0 with
    | Tag_Int -> Stack.pop_int t.stack
    | _ -> assert false
  in
  let elements = Stack.pop_n t.stack length in
  let value = Value.Array elements in
  Stack.push_value t.stack value;
  call_next_instruction t
;;

let execute_record length =
 fun t ->
  let values = Array.to_list @@ Stack.pop_n t.stack length in
  let keys =
    Stack.pop_n t.stack length
    |> Array.to_list
    |> List.map (function
      | Value.String s -> s
      | _ -> assert false)
  in
  let record = StringMap.of_list @@ List.combine keys values in
  let value = Value.Record record in
  Stack.push_value t.stack value;
  call_next_instruction t
;;

let execute_current_closure t =
  let frame = current_frame t in
  let closure = Value.Closure frame.closure in
  Stack.push_value t.stack closure;
  call_next_instruction t
;;

let execute_closure fn_addr num_free_variables =
 fun t ->
  let fn =
    match Array.get t.constants fn_addr with
    | Value.Function fn -> fn
    | _ -> assert false
  in
  let free_variables = Stack.pop_n t.stack num_free_variables in
  let closure = Value.Closure { fn; free_variables } in
  Stack.push_value t.stack closure;
  call_next_instruction t
;;

let execute_return t =
  let from = t.stack.stack_pointer - 1 in
  let t, frame = pop_frame t in
  Stack.set_pointer t.stack frame.base_pointer;
  let () =
    (* This is the function from the I_Call instruction *)
    Stack.drop t.stack
  in
  Stack.copy_to_top t.stack from;
  call_next_instruction t
;;

let execute_halt t = t

let resolve_instructions instructions =
  Array.map
    (fun instruction ->
      match instruction with
      | Instruction.I_Jump addr -> execute_jump addr
      | Instruction.I_Jump_If_False addr -> execute_jump_if_false addr
      | Instruction.I_Constant addr -> execute_constant addr
      | Instruction.I_Debug_Print_Stack -> execute_debug_print_stack
      | Instruction.I_Set_Global addr -> execute_set_global addr
      | Instruction.I_Get_Global addr -> execute_get_global addr
      | Instruction.I_Get_Builtin addr -> execute_get_builtin addr
      | Instruction.I_Get_Free addr -> execute_get_free addr
      | Instruction.I_Range -> execute_binary_range ~inclusive:false
      | Instruction.I_Range_Inclusive -> execute_binary_range ~inclusive:true
      | Instruction.I_Record length -> execute_record length
      | Instruction.I_Closure (fn_addr, num_free_variables) ->
          execute_closure fn_addr num_free_variables
      | Instruction.I_Call num_arguments -> execute_function_call num_arguments
      | Instruction.I_Set_Local addr -> execute_set_local addr
      | Instruction.I_Get_Local addr -> execute_get_local addr
      | Instruction.I_Pop -> execute_pop
      | Instruction.I_Add -> execute_binary_add
      | Instruction.I_Sub -> execute_binary_sub
      | Instruction.I_Div -> execute_binary_div
      | Instruction.I_Mul -> execute_binary_times
      | Instruction.I_Mod -> execute_binary_mod
      | Instruction.I_Pow -> execute_binary_pow
      | Instruction.I_True -> execute_true
      | Instruction.I_False -> execute_false
      | Instruction.I_Equal -> execute_binary_equal
      | Instruction.I_Not_Equal -> execute_binary_not_equal
      | Instruction.I_Greater -> execute_binary_greater
      | Instruction.I_Greater_Equal -> execute_binary_greater_equal
      | Instruction.I_Less -> execute_binary_less
      | Instruction.I_Less_Equal -> execute_binary_less_equal
      | Instruction.I_And -> execute_binary_and
      | Instruction.I_Or -> execute_binary_or
      | Instruction.I_Concat -> execute_binary_concat
      | Instruction.I_Index -> execute_binary_bracket_access
      | Instruction.I_Dot_Index -> execute_binary_dot_access
      | Instruction.I_Minus -> execute_unary_minus
      | Instruction.I_Not -> execute_unary_not
      | Instruction.I_Null -> execute_null
      | Instruction.I_Array -> execute_array
      | Instruction.I_Return -> execute_return
      | Instruction.I_Length -> execute_length
      | Instruction.I_Current_Closure -> execute_current_closure
      | Instruction.I_Halt -> execute_halt)
    instructions
;;

let resolve_constant_functions ~function_count constants =
  (* We add one to the function count to reserve the first space for the main function *)
  let resolved_functions = Array.make function_count [||] in
  let rec resolve_from_value value =
    match value with
    | Value.Null -> ()
    | Value.Int _ -> ()
    | Value.Float _ -> ()
    | Value.Bool _ -> ()
    | Value.Char _ -> ()
    | Value.String _ -> ()
    | Value.Array a -> Array.iter resolve_from_value a
    | Value.Record r -> StringMap.iter (fun _ -> resolve_from_value) r
    | Value.Function fn ->
        let instructions = resolve_instructions fn.instructions in
        Array.set resolved_functions fn.fn_addr instructions
    | Value.Closure closure ->
        let instructions = resolve_instructions closure.fn.instructions in
        Array.set resolved_functions closure.fn.fn_addr instructions
    | Value.BuiltinFunction _ -> ()
  in
  Array.iter resolve_from_value constants;
  resolved_functions
;;

let eval bytecode =
  let function_count = ref 0 in
  let code = bytecode |> Pinc_Bytecode.Bytecode.deserialize ~function_count in
  let instructions =
    Array.append (resolve_instructions code.instructions) [| execute_halt |]
  in
  let constants = Dynarray.to_array @@ code.constants in
  let resolved_functions =
    resolve_constant_functions ~function_count:!function_count constants
  in
  let t = make ~resolved_functions ~instructions ~constants in
  let result = call_current_instruction t in
  result.stack |> Stack.last_popped_element |> Value.to_string
;;
