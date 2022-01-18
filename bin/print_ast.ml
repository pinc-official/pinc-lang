open Fennek_lib

let print_ast (ast : Ast.t) =
  print_endline (Ast.show ast)

let file_contents chan = really_input_string chan (in_channel_length chan)

let main = begin
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let src = file_contents chan in
  let start_time = Unix.gettimeofday() in

  let parser = Parser.make ~filename src in
  let ast = Parser.scan parser in

  let end_time = Unix.gettimeofday() in
  let _seconds = end_time -. start_time in
  print_ast ast;
  close_in chan;
  (* Printf.printf "%fns" (seconds *. 1000. *. 1000.); *)
end

let () = main