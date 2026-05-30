let code_of_color = function
  | Some `Red -> "31"
  | Some `Yellow -> "33"
  | None -> ""
;;

let wrap ~bold ?color text =
  let styles = [] in
  let styles =
    match code_of_color color with
    | "" -> styles
    | color -> color :: styles
  in
  let styles =
    if bold then
      "1" :: styles
    else
      styles
  in
  let style_codes = String.concat ";" styles in
  Printf.sprintf "\027[%sm%s\027[0m" style_codes text
;;

let faint = Printf.sprintf "\027[2m%s\027[22m"
