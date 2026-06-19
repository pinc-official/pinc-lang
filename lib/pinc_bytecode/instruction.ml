type t =
  | I_Pop
  | I_Constant of UInt16.t
  | I_Add
  | I_Sub
  | I_Div
  | I_Mul
  | I_Mod
  | I_Pow

let byte = function
  | I_Pop -> 0x00
  | I_Constant _ -> 0x01
  | I_Add -> 0x02
  | I_Sub -> 0x03
  | I_Div -> 0x04
  | I_Mul -> 0x05
  | I_Mod -> 0x06
  | I_Pow -> 0x07
;;

let operands_length = function
  | I_Pop -> 0
  | I_Constant addr -> UInt16.width addr
  | I_Add -> 0
  | I_Sub -> 0
  | I_Div -> 0
  | I_Mul -> 0
  | I_Mod -> 0
  | I_Pow -> 0
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
    | I_Pop -> ()
    | I_Constant addr -> offset := UInt16.write bytes !offset addr
    | I_Add -> ()
    | I_Sub -> ()
    | I_Div -> ()
    | I_Mul -> ()
    | I_Mod -> ()
    | I_Pow -> ()
  in

  bytes
;;
