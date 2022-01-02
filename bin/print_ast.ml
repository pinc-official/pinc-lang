open Fennek_lib

let print_ast (ast : Ast.file) =
  print_endline (Ast.show_file ast)

let parse_and_print p = print_ast (Parser.scan p)

let file_contents chan = really_input_string chan (in_channel_length chan)

let main =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let src = file_contents chan in
  let parser = Parser.make ~filename src in
  let () = parse_and_print parser in
  close_in chan

let () = main