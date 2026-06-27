type t = {
  base_pointer : int;
  instructions : Bytes.t;
  mutable instruction_pointer : int;
}

let make base_pointer instructions =
  { base_pointer; instructions; instruction_pointer = 0 }
;;

let instructions t = t.instructions
let set_instruction_pointer t i = t.instruction_pointer <- i
