type t = {
  instructions : Bytes.t;
  mutable pointer : int;
}

let make instructions = { instructions; pointer = 0 }
let instructions t = t.instructions
let set_pointer t i = t.pointer <- i
