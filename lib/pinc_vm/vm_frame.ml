type t = {
  base_pointer : int;
  mutable instruction_pointer : int;
  instructions : Pinc_Bytecode.Instruction.t Array.t;
  closure : Pinc_Bytecode.Value.closure;
}

let make ~base_pointer ~closure =
  {
    base_pointer;
    closure;
    instructions = closure.Pinc_Bytecode.Value.fn.instructions;
    instruction_pointer = 0;
  }
;;

let instructions t = t.instructions
let free_variables t = t.closure.free_variables
let set_instruction_pointer t i = t.instruction_pointer <- i
