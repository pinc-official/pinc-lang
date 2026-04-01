open Pinc_lang

let main =
  Printexc.record_backtrace false;

  let file = Sys.argv.(1) in
  let source = Source.of_file file in
  try
    let result = Formatter.format [ source ] in
    print_endline result
  with Diagnostics.Pinc_error -> exit 1
;;

let () = main
