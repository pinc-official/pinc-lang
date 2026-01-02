open Pinc_lang

let main =
  let filename = Sys.argv.(1) in
  let root = Sys.argv.(2) in
  let src = filename |> Source.of_file in
  let start_time = Unix.gettimeofday () in
  let end_time_parser = Unix.gettimeofday () in
  let result =
    [ src ]
    |> Interpreter.eval_sources ~tag_data_provider:Helpers.noop_data_provider ~root
  in
  let end_time = Unix.gettimeofday () in
  Printf.printf "Lexer & Parser: %fms\n" ((end_time_parser -. start_time) *. 1000.);
  Printf.printf "Interpreter:    %fms\n" ((end_time -. end_time_parser) *. 1000.);
  ignore result
;;

let () = main
