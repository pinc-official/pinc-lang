let main =
  Printexc.record_backtrace true;

  let directory = Sys.argv.(1) in
  let root = Sys.argv.(2) in
  Pinc.run ~directory ~root |> print_endline
;;

let () = main
