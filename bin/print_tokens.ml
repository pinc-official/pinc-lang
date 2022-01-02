let file_contents chan = really_input_string chan (in_channel_length chan)

let main =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let src = file_contents chan in
  let lexer = Fennek_lib.Lexer.make ~filename src in
  let lexbuf = ref (Fennek_lib.Lexer.scan lexer) in
  let () = while match !lexbuf.typ with | Fennek_lib.Token.END_OF_INPUT -> false | _ -> true do
    print_endline (Fennek_lib.Token.show !lexbuf);
    lexbuf := Fennek_lib.Lexer.scan lexer
  done in
  close_in chan

let () = main