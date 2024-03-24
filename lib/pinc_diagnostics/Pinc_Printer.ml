module Source = Pinc_Core.Source
module Location = Pinc_Location
module Position = Pinc_Position

let _seek_lines_before ~count source_code pos =
  let original_line = pos.Position.line in
  source_code
  |> String.fold_left
       (fun (current_line, current_char) curr ->
         if current_line + count >= original_line then
           (current_line, current_char)
         else (
           match curr with
           | '\n' -> (current_line + 1, current_char + 1)
           | _ -> (current_line, current_char + 1)))
       (1, 0)
;;

let _seek_lines_after ~count source_code pos =
  let original_line = pos.Position.line in
  source_code
  |> String.fold_left
       (fun (line_number, char_number) curr ->
         if line_number - count - 1 >= original_line then
           (line_number, char_number)
         else (
           match curr with
           | '\n' -> (line_number + 1, char_number + 1)
           | _ -> (line_number, char_number + 1)))
       (1, 0)
;;

let print_code ~color ~loc source_code =
  let context_lines = 1 in
  let start_pos = loc.Location.loc_start in
  let end_pos = loc.Location.loc_end in
  let highlight_line_start = start_pos.line in
  let highlight_line_end = end_pos.line in
  let highlight_column_start = start_pos.column in
  let highlight_column_end = end_pos.column in

  let first_shown_line = start_pos.line - context_lines |> Int.max 0 in
  let last_shown_line = end_pos.line + context_lines in

  let lines =
    source_code
    |> String.split_on_char '\n'
    |> List.filteri (fun index _line ->
           let line_number = succ index in
           line_number >= first_shown_line && line_number <= last_shown_line)
    |> List.mapi (fun i line ->
           let line_number = i + first_shown_line in
           (line_number, line))
    (* try
         String.sub source_code start_char_offset (end_char_offset - start_char_offset)
         |> String.split_on_char '\n'
         |> List.mapi (fun i line ->
                let line_number = i + first_shown_line in
                (line_number, line))
       with Invalid_argument _ -> [] *)
  in

  let buf = Buffer.create 400 in
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
         |> String.iteri (fun column_index ch ->
                let column_number = column_index + 1 in
                let on_line_start = Int.equal line_number highlight_line_start in
                let after_column_start = column_number >= highlight_column_start in
                let between_lines =
                  line_number >= highlight_line_start && line_number <= highlight_line_end
                in
                let between_columns =
                  column_number >= highlight_column_start
                  && column_number <= highlight_column_end
                in
                let on_line_end = Int.equal line_number highlight_line_end in
                let before_column_start = column_number <= highlight_column_end in
                if
                  (on_line_start && (not on_line_end) && after_column_start)
                  || (between_lines && between_columns)
                  || (on_line_end && (not on_line_start) && before_column_start)
                then
                  Fmt.pf ppf "%a" Fmt.(styled `Bold (styled (`Fg color) char)) ch
                else
                  Fmt.pf ppf "%a" Fmt.(styled `None char) ch);
         Format.pp_print_newline ppf ());

  Buffer.contents buf
;;

let print_loc ppf (loc : Location.t) =
  match (Source.name loc.loc_start.source, loc = Location.none) with
  | None, _ | _, true -> ()
  | Some filename, _ ->
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
        (Printf.sprintf "in file %s:%s" filename loc_string)
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

  let source_code = Source.content loc.loc_start.source in
  if source_code <> "" then
    Fmt.pf ppf "@,%s" (print_code ~color ~loc source_code)
;;
