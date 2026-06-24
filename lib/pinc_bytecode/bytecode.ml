type t = {
  instructions : Bytes.t;
  constants : Value.t Int32.Map.t;
}

let make ~instructions ~constants = { instructions; constants }

let pp_instructions fmt t =
  let offset = ref 0 in
  while !offset < Bytes.length t.instructions do
    let new_offset, t = Instruction.decode t.instructions !offset in
    Format.fprintf fmt "%0.4i %a\n%!" !offset Instruction.pp t;
    offset := new_offset
  done
;;
