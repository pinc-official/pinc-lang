open Fennek_lib

let main =
  let filename = Sys.argv.(1) in
  let result = Interpreter.from_file ~filename () in
  print_endline result
;;

let () = main