open Fennek_lib

let file_contents chan = really_input_string chan (in_channel_length chan)

let main =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let src = chan |> file_contents in
  let start_time = Unix.gettimeofday () in
  let lexer = Lexer.make ~filename src in
  let rec loop lexer =
    match Lexer.scan lexer with
    | { typ = Token.END_OF_INPUT; _ } -> ()
    | _ -> loop lexer
  in
  let () = loop lexer in
  let end_time = Unix.gettimeofday () in
  let seconds = end_time -. start_time in
  Printf.printf "%fms" (seconds *. 1000.);
  close_in chan
;;

let () = main
