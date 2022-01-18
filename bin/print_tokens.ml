open Fennek_lib

let print_token (token : Token.t) =
  print_endline (Token.show token)

let file_contents chan = really_input_string chan (in_channel_length chan)

let main = begin
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let src = file_contents chan in

  let lexer = Lexer.make ~filename src in

  let rec loop lexer = match Lexer.scan lexer with
    | { typ = Token.END_OF_INPUT; _ } as token -> print_token token
    | token -> print_token token; loop lexer
  in
  let () = loop lexer in

  close_in chan;
end

let () = main