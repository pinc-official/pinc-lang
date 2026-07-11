type t = {
  instructions : Instruction.t Array.t;
  constants : Value.t Dynarray.t;
}

let make ~instructions ~constants = { instructions; constants }

let pp_instructions fmt =
  let offset = ref 0 in
  Array.iter @@ fun instruction ->
  if !offset = 0 then
    Format.fprintf fmt "%0.4i %a" !offset Instruction.pp instruction
  else
    Format.fprintf fmt "@;%0.4i %a" !offset Instruction.pp instruction;
  offset := !offset + Instruction.length instruction
;;

let rec pp_value fmt = function
  | Value.Null -> Format.fprintf fmt "<NULL>"
  | Value.Int i -> Format.fprintf fmt "%i" i
  | Value.Float f -> Format.fprintf fmt "%f" f
  | Value.Bool b -> Format.fprintf fmt "%b" b
  | Value.String s -> Format.fprintf fmt "%S" s
  | Value.Char c ->
      let buf = Buffer.create 32 in
      Buffer.add_utf_8_uchar buf c;
      Format.fprintf fmt "'%s'" @@ Buffer.contents buf
  | Value.Array _ -> Format.fprintf fmt "<ARRAY>"
  | Value.Record _ -> Format.fprintf fmt "<RECORD>"
  | Value.Function fn ->
      Format.fprintf fmt "<FUNCTION>";
      pp_function fmt fn
  | Value.Closure { free_variables = _; fn } ->
      Format.fprintf fmt "<CLOSURE>";
      pp_function fmt fn
  | Value.BuiltinFunction _ -> Format.fprintf fmt "<BUILTIN_FUNCTION>"

and pp_function fmt fn =
  Format.fprintf fmt " [@;@[<v2>  %a@]@;]" pp_instructions fn.instructions
;;

let pp_constants fmt constants =
  Dynarray.iteri
    (fun key value ->
      Format.fprintf fmt "@[<v0>0x%08X (%08i) : %a@;@]" key key pp_value value)
    constants
;;

let pp fmt t =
  let () =
    match Dynarray.is_empty t.constants with
    | true -> ()
    | false ->
        Format.fprintf Format.std_formatter "[CONSTANTS]@.";
        pp_constants fmt t.constants;
        Format.fprintf Format.std_formatter "@."
  in
  let () =
    match t.instructions with
    | [||] -> ()
    | instructions ->
        Format.fprintf Format.std_formatter "[INSTRUCTIONS]@.";
        Format.fprintf fmt "@[<v0>%a@]" pp_instructions instructions
  in
  ()
;;

let serialize t =
  let buf = Buffer.create 65565 in
  let constants = t.constants in
  let num_constants = Dynarray.length constants in
  Buffer.add_int32_be buf @@ Int32.of_int num_constants;
  let () = constants |> Dynarray.iter (Value.serialize buf) in
  let () = Buffer.add_int32_be buf @@ Int32.of_int (Array.length t.instructions) in
  let () =
    Array.iter
      (fun instruction ->
        let serialized = Instruction.to_bytes instruction in
        Buffer.add_bytes buf serialized)
      t.instructions
  in
  Buffer.contents buf
;;

let deserialize ?(function_count = ref 0) str =
  let bytes = Bytes.of_string str in
  let offset = ref 0 in
  let num_constants = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let constants =
    Dynarray.init num_constants @@ fun _ -> Value.deserialize ~function_count bytes offset
  in
  let instructions_length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let instructions =
    Array.init instructions_length (fun _ ->
        let new_offset, res = Instruction.decode bytes !offset in
        offset := new_offset;
        res)
  in
  assert (!offset = Bytes.length bytes);
  make ~instructions ~constants
;;
