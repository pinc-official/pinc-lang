open Pinc_lang

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

let get_sources_from ~directory () =
  directory |> get_files_with_ext ~ext:".pi" |> List.map Source.of_file
;;

let main =
  Printexc.record_backtrace false;

  let directory = Sys.argv.(1) in
  let root = Sys.argv.(2) in
  let sources = get_sources_from ~directory () in
  try
    sources
    |> Parser.get_ast ~include_stdlib:false
    |> StringMap.find root
    |> StringMap.singleton root
    |> Compiler.compile
    |> Bytecode.pp_instructions Format.std_formatter
  with Diagnostics.Pinc_error _ -> exit 1
;;

let () = main
