open Pinc_Bytecode

exception Pinc_stack_overflow

type t = {
  bytecode : Bytecode.t;
  mutable stack_pointer : int;
  mutable stack : Value.t Array.t;
}

let stack_size = 2048

let make (bytecode : Bytecode.t) =
  { bytecode; stack_pointer = 0; stack = Array.make stack_size Value.Null }
;;

let push t value =
  if t.stack_pointer > stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    t.stack.(t.stack_pointer) <- value;
    t.stack_pointer <- succ t.stack_pointer)
;;

let pop t =
  if t.stack_pointer == 0 then
    assert false
  else (
    let value = t.stack.(t.stack_pointer - 1) in
    t.stack_pointer <- pred t.stack_pointer;
    value)
;;

let stack_top t =
  match t.stack_pointer with
  | 0 -> Value.Null
  | n -> t.stack.(n - 1)
;;

let last_popped_stack_element t = t.stack.(t.stack_pointer)

let run t =
  let ip = ref 0 in
  let instruction_length = Bytes.length t.bytecode.instructions in
  while !ip < instruction_length do
    let new_ip, op = Instruction.decode t.bytecode.instructions !ip in
    let () =
      match op with
      | Instruction.I_Pop -> ignore @@ pop t
      | Instruction.I_Constant addr ->
          let constant = UInt16.Map.find addr t.bytecode.constants in
          push t constant
      | Instruction.I_Add ->
          let r = pop t in
          let l = pop t in
          let result =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int (l + r)
            | _ -> assert false (* TODO *)
          in
          push t result
    in
    ip := new_ip
  done;
  t
;;

let eval bytecode =
  bytecode |> make |> run |> last_popped_stack_element |> Value.to_string
;;
