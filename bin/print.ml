open Pinc_lib

let main =
  let directory = Sys.argv.(1) in
  let root_name = Sys.argv.(2) in
  let result = Interpreter.from_directory ~directory root_name in
  result |> Interpreter.State.get_output |> Interpreter.Value.to_string |> print_endline
;;

let () = main
