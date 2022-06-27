open Pinc

let get_source filename =
  let file_contents chan = really_input_string chan (in_channel_length chan) in
  let chan = open_in filename in
  let src = chan |> file_contents in
  close_in chan;
  src
;;

let main =
  let filename = Sys.argv.(1) in
  let root = Sys.argv.(2) in
  let src = filename |> get_source in
  let start_time = Unix.gettimeofday () in
  let ast = src |> Parser.parse ~filename in
  let end_time_parser = Unix.gettimeofday () in
  let result =
    ast
    |> Interpreter.eval ~root
    |> Interpreter.State.get_output
    |> Interpreter.Value.to_string
  in
  let end_time = Unix.gettimeofday () in
  Printf.printf "Lexer & Parser: %fms\n" ((end_time_parser -. start_time) *. 1000.);
  Printf.printf "Interpreter:    %fms\n" ((end_time -. end_time_parser) *. 1000.);
  ignore result
;;

let () = main
