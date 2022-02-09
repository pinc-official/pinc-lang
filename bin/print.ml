open Pinc_lib

let main =
  let directory = Sys.argv.(1) in
  let root_name = Sys.argv.(2) in
  let result = Interpreter.from_directory ~directory root_name in
  print_endline result
;;

let () = main
