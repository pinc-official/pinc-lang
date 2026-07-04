module Option = struct
  include Option

  let fold_map t ~init ~f =
    let acc = ref init in
    let result =
      t
      |> map @@ fun x ->
         let new_acc, y = f !acc x in
         acc := new_acc;
         y
    in
    (!acc, result)
  ;;
end

module List = struct
  include List

  let fold_map t ~init ~f =
    let acc = ref init in
    let result =
      t
      |> map @@ fun x ->
         let new_acc, y = f !acc x in
         acc := new_acc;
         y
    in
    (!acc, result)
  ;;

  let rec last list =
    match list with
    | [ x ] -> Some x
    | _ :: tl -> last tl
    | [] -> None
  ;;
end

module Array = struct
  include Array

  let fold_map t ~init ~f =
    let acc = ref init in
    let result =
      t
      |> map @@ fun x ->
         let new_acc, y = f !acc x in
         acc := new_acc;
         y
    in
    (!acc, result)
  ;;
end

module Char = struct
  include Char

  let[@inline] is_whitespace = function
    | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' ->
        true
    | _ -> false
  ;;
end

module String = struct
  include String

  let[@inline] subrange ?(first = 0) ?(last = max_int) s =
    (* assert (Sys.max_string_length - 1 < max_int) *)
    let max = length s - 1 in
    let first =
      if first < 0 then
        0
      else
        first
    in
    let last =
      if last > max then
        max
      else
        last
    in
    if first > last then
      ""
    else
      sub s first (last - first + 1)
  ;;

  let take_first n s = subrange ~last:(n - 1) s
  let drop_first n s = subrange ~first:n s
  let cut_first n s = (take_first n s, drop_first n s)
  let take_last n s = subrange ~first:(length s - n) s
  let drop_last n s = subrange ~last:(length s - n - 1) s
  let cut_last n s = (drop_last n s, take_last n s)

  let drop_prefix ~prefix s =
    if starts_with ~prefix s then
      Some (drop_first (length prefix) s)
    else
      None
  ;;

  let drop_suffix ~suffix s =
    if ends_with ~suffix s then
      Some (drop_last (length suffix) s)
    else
      None
  ;;

  let find_index s fn =
    let seq = String.to_seq s in
    Seq.find_index fn seq
  ;;

  let indentation s =
    let index =
      find_index s (fun c -> not (Char.is_whitespace c)) |> Option.value ~default:0
    in
    take_first index s
  ;;

  let trim_left s =
    let len = length s in
    let i = ref 0 in
    while !i < len && Char.is_whitespace (unsafe_get s !i) do
      incr i
    done;
    let j = len - 1 in
    if j >= !i then
      sub s !i (j - !i + 1)
    else
      empty
  ;;

  let trim_right s =
    let len = length s in
    let i = 0 in
    let j = ref (len - 1) in
    while !j >= i && Char.is_whitespace (unsafe_get s !j) do
      decr j
    done;
    if !j >= i then
      sub s i (!j - i + 1)
    else
      empty
  ;;

  let to_hex d =
    let char_hex n =
      Char.chr
        (if n < 10 then
           Char.code '0' + n
         else
           Char.code 'a' + n - 10)
    in
    let len = String.length d in
    let result = Bytes.create (len * 2) in
    for i = 0 to len - 1 do
      let x = Char.code d.[i] in
      Bytes.unsafe_set result (i * 2) (char_hex (x lsr 4));
      Bytes.unsafe_set result ((i * 2) + 1) (char_hex (x land 0x0f))
    done;
    Bytes.unsafe_to_string result
  ;;

  let of_hex s =
    let digit c =
      match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | _ -> invalid_arg "String.of_hex"
    in
    let byte i = (digit s.[i] lsl 4) + digit s.[i + 1] in
    String.init (String.length s / 2) (fun i -> Char.chr (byte (2 * i)))
  ;;
end

module Bytes = struct
  include Bytes

  let to_hex t = t |> Bytes.to_string |> String.to_hex
  let of_hex str = str |> String.of_hex |> Bytes.of_string

  let pp_hum t =
    let buf = Buffer.create 16 in
    Bytes.iter (fun c -> Printf.bprintf buf "0x%02X " (Char.code c)) t;
    String.trim (Buffer.contents buf)
  ;;
end

module Buffer = struct
  include Buffer

  let pp_bytes t = t |> Buffer.to_bytes |> Bytes.pp_hum
end

module Int32 = struct
  include Int32

  let byte_width = 4

  let write_bytes bytes offset t =
    Bytes.set_int32_be bytes offset t;
    offset + byte_width
  ;;

  let read_bytes bytes offset =
    let result = Bytes.get_int32_be bytes offset in
    (offset + byte_width, result)
  ;;

  let pp fmt t = Format.fprintf fmt "0x%08lX (%08li)" t t

  module Map = Map.Make (Int32)
end
