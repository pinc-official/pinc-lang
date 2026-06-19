type t =
  | I_Constant of UInt16.t
  | I_Add

let byte = function
  | I_Constant _ -> 0x01
  | I_Add -> 0x02
;;

let operands_length = function
  | I_Constant addr -> UInt16.width addr
  | I_Add -> 0
;;

let decode bytes offset =
  let instruction = Bytes.get_uint8 bytes offset in
  let offset = offset + 1 in
  match instruction with
  | 0x01 ->
      let offset, addr = UInt16.read bytes offset in
      (offset, I_Constant addr)
  | 0x02 -> (offset, I_Add)
  | _ ->
      raise_notrace
        (Invalid_argument (Printf.sprintf "unknown instruction: 0x%.2X" instruction))
;;

let pp fmt = function
  | I_Constant addr -> Format.fprintf fmt "I_Constant %a" UInt16.pp addr
  | I_Add -> Format.fprintf fmt "I_Add"
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
    | I_Constant addr -> offset := UInt16.write bytes !offset addr
    | I_Add -> ()
  in

  bytes
;;
