type t = {
  base_pointer : int;
  closure : Pinc_Bytecode.Value.closure;
  mutable instruction_pointer : int;
}

let make ~base_pointer ~closure = { base_pointer; closure; instruction_pointer = 0 }
let instructions t = t.closure.fn.instructions
let free_variables t = t.closure.free_variables
let set_instruction_pointer t i = t.instruction_pointer <- i
