open Js_of_ocaml
open Pinc_Core

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
  directory |> get_files_with_ext ~ext:".pi" |> List.map Pinc_Source.of_file
;;

let compile input out =
  let declarations = get_declarations_from ~directory:input () in
  let ast = Pinc_Parser.get_ast declarations in
  let dependency_graph = Pinc_Parser.DependencyGraph.build ast in

  ast
  |> StringMap.iter (fun key declaration ->
      let code =
        declaration |> StringMap.singleton key |> Pinc_Types.Ast.marshal |> String.to_hex
      in
      let component_dependencies =
        Pinc_Parser.DependencyGraph.dependencies_of dependency_graph key
        |> StringSet.elements
      in

      let imports =
        component_dependencies
        |> List.map (fun key ->
            let path = Printf.sprintf "./%s.pi.mjs" key in
            Printf.sprintf "import { __internal_code as %s } from %S;\n" key path)
        |> String.concat ""
      in

      let code_export = Printf.sprintf "export const __internal_code = %S;\n" code in

      let code =
        component_dependencies
        |> List.cons "__internal_code"
        |> List.rev
        |> String.concat ","
      in
      let export = Printf.sprintf "export default [%S,%s];" key code in
      let result = imports ^ code_export ^ export in

      let filename = Printf.sprintf "%s.pi.mjs" key in
      Out_channel.(
        with_open_bin (Filename.concat out filename) (fun oc -> output_string oc result)))
;;

let () = Js.export "compile" compile
