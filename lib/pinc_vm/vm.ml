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
      | Instruction.I_Add ->
          let r = Stack.pop t.stack in
          let l = Stack.pop t.stack in
          let result =
            match (l, r) with
            | Value.Int l, Value.Int r -> Value.Int (l + r)
            | _ -> assert false (* TODO *)
          in
          Stack.push t.stack result
    in
    ip := new_ip
  done;
  t
;;

let eval bytecode =
  let vm = bytecode |> make |> run in
  vm.stack |> Stack.last_popped_element |> Value.to_string
;;
