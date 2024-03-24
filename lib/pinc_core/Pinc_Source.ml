type t =
  | Empty
  | File of string
  | String of string option * string

let empty = Empty
let of_string ?filename content = String (filename, content)
let of_file path = File path

let name = function
  | Empty -> None
  | File name -> Some name
  | String (name, _) -> name
;;

let length = function
  | Empty -> 0
  | File filename -> (
      try In_channel.(with_open_bin filename length) |> Int64.to_int
      with _ -> invalid_arg "file size is larger than an OCaml 63-bit integer")
  | String (_name, content) -> String.length content
;;

let content = function
  | Empty -> ""
  | File filename -> In_channel.(with_open_bin filename input_all)
  | String (_name, content) -> content
;;
