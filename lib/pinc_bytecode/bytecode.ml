type t = {
  instructions : Bytes.t;
  constants : Value.t Int32.Map.t;
}

let make ~instructions ~constants = { instructions; constants }

let pp_instructions fmt instructions =
  let offset = ref 0 in
  while !offset < Bytes.length instructions do
    let new_offset, t = Instruction.decode instructions !offset in
    if !offset = 0 then
      Format.fprintf fmt "%0.4i %a" !offset Instruction.pp t
    else
      Format.fprintf fmt "@;%0.4i %a" !offset Instruction.pp t;
    offset := new_offset
  done
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
  Int32.Map.iter
    (fun key value -> Format.fprintf fmt "@[<v0>%a : %a@;@]" Int32.pp key pp_value value)
    constants
;;

let pp fmt t =
  if not @@ Int32.Map.is_empty t.constants then (
    Format.fprintf Format.std_formatter "[CONSTANTS]@.";
    pp_constants fmt t.constants;
    Format.fprintf Format.std_formatter "@.");

  if Bytes.length t.instructions > 0 then (
    Format.fprintf Format.std_formatter "[INSTRUCTIONS]@.";
    Format.fprintf fmt "@[<v0>%a@]" pp_instructions t.instructions)
;;

let serialize t =
  let buf = Buffer.create 65565 in
  let constants = t.constants in
  let num_constants = Int32.Map.cardinal constants in
  Buffer.add_int32_be buf @@ Int32.of_int num_constants;
  let () =
    constants
    |> Int32.Map.iter (fun key value ->
        Buffer.add_int32_be buf key;
        Value.serialize buf value)
  in
  let () = Buffer.add_int32_be buf @@ Int32.of_int (Bytes.length t.instructions) in
  let () = Buffer.add_bytes buf t.instructions in
  Buffer.contents buf
;;

let deserialize str =
  let bytes = Bytes.of_string str in
  let offset = ref 0 in
  let num_constants = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let constants =
    Int32.Map.of_seq
    @@ Seq.init num_constants (fun _ ->
        let key = Bytes.get_int32_be bytes !offset in
        offset := !offset + 4;
        let value = Value.deserialize bytes offset in
        (key, value))
  in
  let instructions_length = Int32.to_int @@ Bytes.get_int32_be bytes !offset in
  offset := !offset + 4;
  let instructions = Bytes.sub bytes !offset instructions_length in
  offset := !offset + instructions_length;
  assert (!offset = Bytes.length bytes);
  make ~instructions ~constants
;;
