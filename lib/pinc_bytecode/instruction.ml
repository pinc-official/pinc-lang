type t =
  | I_Null
  | I_Pop
  | I_Constant of UInt16.t
  | I_Add
  | I_Sub
  | I_Div
  | I_Mul
  | I_Mod
  | I_Pow
  | I_True
  | I_False
  | I_Equal
  | I_Not_Equal
  | I_Greater
  | I_Greater_Equal
  | I_Less
  | I_Less_Equal
  | I_And
  | I_Or
  | I_Minus
  | I_Not
  | I_Jump of UInt16.t
  | I_Jump_If_False of UInt16.t
  | I_Set_Global of UInt16.t
  | I_Get_Global of UInt16.t

let byte = function
  | I_Pop -> 0x00
  | I_Constant _ -> 0x01
  | I_Add -> 0x02
  | I_Sub -> 0x03
  | I_Div -> 0x04
  | I_Mul -> 0x05
  | I_Mod -> 0x06
  | I_Pow -> 0x07
  | I_True -> 0x08
  | I_False -> 0x09
  | I_Equal -> 0x0A
  | I_Not_Equal -> 0x0B
  | I_Greater -> 0x0C
  | I_Greater_Equal -> 0x0D
  | I_Less -> 0x0E
  | I_Less_Equal -> 0x0F
  | I_And -> 0x10
  | I_Or -> 0x11
  | I_Minus -> 0x12
  | I_Not -> 0x13
  | I_Jump _ -> 0x14
  | I_Jump_If_False _ -> 0x15
  | I_Null -> 0x16
  | I_Set_Global _ -> 0x17
  | I_Get_Global _ -> 0x18
;;

let operands_length = function
  | I_Constant op | I_Jump op | I_Jump_If_False op | I_Set_Global op | I_Get_Global op ->
      UInt16.width op
  | I_Pop
  | I_Add
  | I_Sub
  | I_Div
  | I_Mul
  | I_Mod
  | I_Pow
  | I_True
  | I_False
  | I_Equal
  | I_Not_Equal
  | I_Greater
  | I_Greater_Equal
  | I_Less
  | I_Less_Equal
  | I_And
  | I_Or
  | I_Minus
  | I_Not
  | I_Null -> 0
;;

let decode bytes offset =
  let instruction = Bytes.get_uint8 bytes offset in
  let offset = offset + 1 in
  match instruction with
  | 0x00 -> (offset, I_Pop)
  | 0x01 ->
      let offset, addr = UInt16.read bytes offset in
      (offset, I_Constant addr)
  | 0x02 -> (offset, I_Add)
  | 0x03 -> (offset, I_Sub)
  | 0x04 -> (offset, I_Div)
  | 0x05 -> (offset, I_Mul)
  | 0x06 -> (offset, I_Mod)
  | 0x07 -> (offset, I_Pow)
  | 0x08 -> (offset, I_True)
  | 0x09 -> (offset, I_False)
  | 0x0A -> (offset, I_Equal)
  | 0x0B -> (offset, I_Not_Equal)
  | 0x0C -> (offset, I_Greater)
  | 0x0D -> (offset, I_Greater_Equal)
  | 0x0E -> (offset, I_Less)
  | 0x0F -> (offset, I_Less_Equal)
  | 0x10 -> (offset, I_And)
  | 0x11 -> (offset, I_Or)
  | 0x12 -> (offset, I_Minus)
  | 0x13 -> (offset, I_Not)
  | 0x14 ->
      let offset, addr = UInt16.read bytes offset in
      (offset, I_Jump addr)
  | 0x15 ->
      let offset, addr = UInt16.read bytes offset in
      (offset, I_Jump_If_False addr)
  | 0x16 -> (offset, I_Null)
  | 0x17 ->
      let offset, addr = UInt16.read bytes offset in
      (offset, I_Set_Global addr)
  | 0x18 ->
      let offset, addr = UInt16.read bytes offset in
      (offset, I_Get_Global addr)
  | _ ->
      raise_notrace
        (Invalid_argument (Printf.sprintf "unknown instruction: 0x%.2X" instruction))
;;

let pp fmt = function
  | I_Pop -> Format.fprintf fmt "I_Pop"
  | I_Constant addr -> Format.fprintf fmt "I_Constant %a" UInt16.pp addr
  | I_Add -> Format.fprintf fmt "I_Add"
  | I_Sub -> Format.fprintf fmt "I_Sub"
  | I_Div -> Format.fprintf fmt "I_Div"
  | I_Mul -> Format.fprintf fmt "I_Mul"
  | I_Mod -> Format.fprintf fmt "I_Mod"
  | I_Pow -> Format.fprintf fmt "I_Pow"
  | I_True -> Format.fprintf fmt "I_True"
  | I_False -> Format.fprintf fmt "I_False"
  | I_Equal -> Format.fprintf fmt "I_Equal"
  | I_Not_Equal -> Format.fprintf fmt "I_Not_Equal"
  | I_Greater -> Format.fprintf fmt "I_Greater"
  | I_Greater_Equal -> Format.fprintf fmt "I_Greater_Equal"
  | I_Less -> Format.fprintf fmt "I_Less"
  | I_Less_Equal -> Format.fprintf fmt "I_Less_Equal"
  | I_And -> Format.fprintf fmt "I_And"
  | I_Or -> Format.fprintf fmt "I_Or"
  | I_Minus -> Format.fprintf fmt "I_Minus"
  | I_Not -> Format.fprintf fmt "I_Not"
  | I_Jump addr -> Format.fprintf fmt "I_Jump %a" UInt16.pp addr
  | I_Jump_If_False addr -> Format.fprintf fmt "I_Jump_If_False %a" UInt16.pp addr
  | I_Null -> Format.fprintf fmt "I_Null"
  | I_Get_Global addr -> Format.fprintf fmt "I_Get_Global %a" UInt16.pp addr
  | I_Set_Global addr -> Format.fprintf fmt "I_Set_Global %a" UInt16.pp addr
;;

let to_bytes t =
  let instruction_length =
    let initial = 1 in
    initial + operands_length t
  in

  let bytes = Bytes.create instruction_length in
  let () = Bytes.set_uint8 bytes 0 (byte t) in
  let offset = ref 1 in

  let () =
    match t with
    | I_Constant op | I_Jump op | I_Jump_If_False op | I_Set_Global op | I_Get_Global op
      -> offset := UInt16.write bytes !offset op
    | I_Pop
    | I_Add
    | I_Sub
    | I_Div
    | I_Mul
    | I_Mod
    | I_Pow
    | I_True
    | I_False
    | I_Equal
    | I_Not_Equal
    | I_Greater
    | I_Greater_Equal
    | I_Less
    | I_Less_Equal
    | I_And
    | I_Or
    | I_Minus
    | I_Not
    | I_Null -> ()
  in

  bytes
;;
