module Location = Pinc_Location
module Position = Pinc_Position

let seek_lines_before ~count ic pos =
  let original_line = pos.Position.line in
  In_channel.seek ic 0L;
  let rec loop current_line current_char =
    if current_line + count >= original_line then
      (current_char, current_line)
    else (
      match In_channel.input_char ic with
      | Some '\n' -> loop (current_line + 1) (current_char + 1)
      | Some _ -> loop current_line (current_char + 1)
      | None -> (current_char, current_line))
  in
  loop 1 0
;;

let seek_lines_after ~count ic pos =
  let original_line = pos.Position.line in
  In_channel.seek ic 0L;
  let rec loop current_line current_char =
    if current_line - count - 1 >= original_line then
      (current_char - 1, current_line)
    else (
      match In_channel.input_char ic with
      | Some '\n' -> loop (current_line + 1) (current_char + 1)
      | Some _ -> loop current_line (current_char + 1)
      | None -> (current_char, current_line))
  in
  loop 1 0
;;

let print_code ~color ~loc ic =
  let context_lines = 1 in
  let start_pos = loc.Location.loc_start in
  let end_pos = loc.Location.loc_end in
  let highlight_line_start = start_pos.line in
  let highlight_line_end = end_pos.line in
  let highlight_column_start = start_pos.column in
  let highlight_column_end = end_pos.column in
  let start_char_offset, first_shown_line =
    seek_lines_before ~count:context_lines ic start_pos
  in
  let end_char_offset, _last_shown_line =
    seek_lines_after ~count:context_lines ic end_pos
  in
  In_channel.seek ic (Int64.of_int start_char_offset);
  let lines =
    In_channel.really_input_string ic (end_char_offset - start_char_offset)
    |> Option.value ~default:""
    |> String.split_on_char '\n'
    |> List.mapi (fun i line ->
           let line_number = i + first_shown_line in
           (line_number, line))
  in
  let buf = Buffer.create 100 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf `Ansi_tty;
  lines
  |> List.iter (fun (line_number, line) ->
         if line_number >= highlight_line_start && line_number <= highlight_line_end then
           Fmt.pf
             ppf
             "%a"
             Fmt.(styled `Bold (styled (`Fg color) (fun ppf -> Fmt.pf ppf "%4d")))
             line_number
         else
           Fmt.pf ppf "%4d" line_number;
         Fmt.pf ppf " %a " Fmt.(styled `Faint string) "│";
         line
         |> String.iteri (fun column_number ch ->
                if
                  line_number = highlight_line_start
                  && column_number >= highlight_column_start - 1
                  || line_number = highlight_line_end
                     && column_number < highlight_column_end - 1
                  || line_number > highlight_line_start
                     && line_number < highlight_line_end
                then
                  Fmt.pf ppf "%a" Fmt.(styled `Bold (styled (`Fg color) char)) ch
                else
                  Fmt.pf ppf "%a" Fmt.(styled `None char) ch);
         Format.pp_print_newline ppf ());
  Buffer.contents buf
;;

let print_loc ppf (loc : Location.t) =
  if loc = Location.none then
    ()
  else (
    let loc_string =
      if loc.loc_start.line = loc.loc_end.line then
        if loc.loc_start.column = loc.loc_end.column then
          Format.sprintf "%i:%i" loc.loc_start.line loc.loc_start.column
        else
          Format.sprintf
            "%i:%i-%i"
            loc.loc_start.line
            loc.loc_start.column
            loc.loc_end.column
      else
        Format.sprintf
          "%i:%i-%i:%i"
          loc.loc_start.line
          loc.loc_start.column
          loc.loc_end.line
          loc.loc_end.column
    in
    Fmt.pf
      ppf
      "%a"
      Fmt.(styled `Faint string)
      (Printf.sprintf "in file %s:%s" loc.loc_start.filename loc_string))
;;

let print_header ppf ~color text =
  Fmt.pf ppf "%a" Fmt.(styled `Bold (styled color string)) text
;;

let print ~kind ppf (loc : Location.t) =
  let color, header =
    match kind with
    | `warning -> (`Yellow, "WARNING")
    | `error -> (`Red, "ERROR")
  in
  Fmt.pf ppf "@[%a@] " (print_header ~color) header;
  Fmt.pf ppf "@[%a@]@," print_loc loc;
  try
    In_channel.with_open_bin loc.loc_start.filename (fun ic ->
        Fmt.pf ppf "@,%s" (print_code ~color ~loc ic))
  with Sys_error _ -> ()
;;
