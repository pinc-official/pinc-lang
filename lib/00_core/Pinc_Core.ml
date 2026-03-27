include StdlibExtension
module StringMap = StringMap
module StringSet = StringSet
module Identifier = Identifier

module Dedent = struct
  let indentation =
    List.fold_left
      (fun min line ->
        match String.find_index line (fun c -> not (Char.is_whitespace c)) with
        | None -> min
        | Some i ->
            Some
              (match min with
              | None -> i
              | Some min -> Int.min i min))
      None
  ;;

  let drop_indentation lines =
    match indentation lines with
    | None -> lines
    | Some indentation -> List.map (fun line -> String.drop_first indentation line) lines
  ;;

  let lines string =
    let lines = String.split_on_char '\n' string in
    let lines = List.map Containers.String.rtrim lines in
    let lines =
      match lines with
      | [] -> []
      | "" :: rest -> drop_indentation rest
      | first :: rest -> Containers.String.ltrim first :: drop_indentation rest
    in
    lines
  ;;

  let string input = String.concat "\n" (lines input)
end
