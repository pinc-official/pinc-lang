module Ast = Pinc_Frontend.Ast
module Parser = Pinc_Frontend.Parser

module Interpreter = struct
  include Pinc_Backend.Pinc_Interpreter
  module Types = Pinc_Backend.Pinc_Interpreter_Types
end

module StringMap = Map.Make (String)
module Typer = Pinc_Backend.Pinc_Typer

let get_files_with_ext ~ext dir =
  let rec loop result = function
    | file :: rest when Eio.Path.is_directory file ->
        let contents = Eio.Path.read_dir file in
        contents
        |> List.map (fun entry -> Eio.Path.(file / entry))
        |> List.append rest
        |> loop result
    | file :: rest ->
        let _, name = file in
        if Filename.extension name = ext then
          loop (file :: result) rest
        else
          loop result rest
    | [] -> result
  in
  loop [] [ dir ]
;;

let get_declarations_from ~fs ~directory ?(ext = ".pi") () =
  let files = Eio.Path.(fs / directory) |> get_files_with_ext ~ext in
  Eio.Switch.run @@ fun sw ->
  StringMap.of_seq
    ( Eio.Fiber.fork_seq ~sw @@ fun yield ->
      files
      |> List.iter @@ fun file ->
         let _, filename = file in
         let declarations = file |> Eio.Path.load |> Parser.parse ~filename in
         declarations |> StringMap.to_list |> List.iter yield )
;;

let run ~directory ~root =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  get_declarations_from ~fs ~directory ()
  |> Interpreter.eval ~env ~root
  |> Interpreter.State.get_output
  |> Interpreter.Value.to_string
;;
