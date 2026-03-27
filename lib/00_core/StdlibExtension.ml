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
end
