type t = {
  base_pointer : int;
  instructions : Bytes.t;
  free_variables : Pinc_Bytecode.Value.t Int32.Map.t;
  mutable instruction_pointer : int;
}

let make ~base_pointer ~instructions ~free_variables =
  { base_pointer; instructions; instruction_pointer = 0; free_variables }
;;

let instructions t = t.instructions
let set_instruction_pointer t i = t.instruction_pointer <- i
