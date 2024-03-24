open Pinc

let get_files_with_ext ~ext dir =
  let rec loop result = function
    | file :: rest when Sys.is_directory file ->
        Sys.readdir file
        |> Array.to_list
        |> List.map (Filename.concat file)
        |> List.append rest
        |> loop result
    | file :: rest when Filename.extension file = ext -> loop (file :: result) rest
    | _file :: rest -> loop result rest
    | [] -> result
  in
  loop [] [ dir ]
;;

let get_declarations_from ~directory () =
  directory |> get_files_with_ext ~ext:".pi" |> List.map Source.of_file
;;

let main =
  Printexc.record_backtrace false;

  let directory = Sys.argv.(1) in
  let root = Sys.argv.(2) in
  let declarations = get_declarations_from ~directory () in
  declarations
  |> Interpreter.eval ~root ~tag_data_provider:Interpreter.noop_data_provider
  |> print_endline
;;

let () = main
