open Pinc

let get_source filename =
  let file_contents chan = really_input_string chan (in_channel_length chan) in
  let chan = open_in filename in
  let src = chan |> file_contents in
  close_in chan;
  src
;;

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
  (* TODO: This should happen asynchronously *)
  directory
  |> get_files_with_ext ~ext:".pi"
  |> List.fold_left
       (fun acc filename ->
         let decls = filename |> get_source |> Parser.parse ~filename in
         let f key x y =
           match (x, y) with
           | None, Some y -> Some y
           | Some x, None -> Some x
           | Some _, Some _ ->
               Pinc_Diagnostics.error
                 (Pinc_Diagnostics.Location.make
                    ~s:
                      (Pinc_Diagnostics.Location.Position.make
                         ~filename
                         ~line:0
                         ~column:0)
                    ())
                 ("Found multiple declarations with identifier " ^ key)
           | None, None -> None
         in
         StringMap.merge f acc decls)
       StringMap.empty
;;

let main =
  Printexc.record_backtrace true;

  let directory = Sys.argv.(1) in
  let root = Sys.argv.(2) in
  let declarations = get_declarations_from ~directory () in
  declarations
  |> Interpreter.eval ~root
  |> Interpreter.State.get_output
  |> Interpreter.Value.to_string
  |> print_endline
;;

let () = main
