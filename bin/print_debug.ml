open Fennek_lib

let main =
  let filename = Sys.argv.(1) in
  let start_time = Unix.gettimeofday () in
  let ast = Parser.parse_file filename in
  let end_time_parser = Unix.gettimeofday () in
  let result = Interpreter.from_ast ast in
  let end_time = Unix.gettimeofday () in
  Printf.printf
    "Lexer & Parser: %fms\n"
    ((end_time_parser -. start_time) *. 1000.);
  Printf.printf "Interpreter:    %fms\n" ((end_time -. end_time_parser) *. 1000.);
  ignore result
;;

let () = main