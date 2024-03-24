module Source = Pinc_Core.Source
module Location = Pinc_Location
module Position = Pinc_Position

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
  in

  let buf = Buffer.create 400 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer
    ppf
    (if color <> `None then
       `Ansi_tty
     else
       `None);

  let () =
    lines
    |> List.iter @@ fun (line_number, line) ->
       if line_number >= highlight_line_start && line_number <= highlight_line_end then
         Fmt.pf
           ppf
           "%a"
           Fmt.(styled `Bold (styled color (fun ppf -> Fmt.pf ppf "%4d")))
           line_number
       else
         Fmt.pf ppf "%4d" line_number;

       Fmt.pf ppf " %a " Fmt.(styled `Faint string) "│";

       let did_highlight = ref false in

       let should_highlight column_number =
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

         (on_line_start && (not on_line_end) && after_column_start)
         || (between_lines && between_columns)
         || (on_line_end && (not on_line_start) && before_column_start)
       in

       let () =
         line
         |> String.iteri @@ fun column_index ch ->
            let column_number = column_index + 1 in
            if should_highlight column_number then (
              Fmt.pf ppf "%a" Fmt.(styled `Bold (styled color char)) ch;
              did_highlight := true)
            else
              Fmt.pf ppf "%a" Fmt.(styled `None char) ch
       in

       let () =
         if !did_highlight && color = `None then (
           Format.pp_print_newline ppf ();
           Fmt.pf ppf "     %a " Fmt.(styled `Faint string) "│";
           line
           |> String.iteri @@ fun column_index _ch ->
              if should_highlight (succ column_index) then
                Fmt.pf ppf "^"
              else
                Fmt.pf ppf " ")
       in

       Format.pp_print_newline ppf ()
  in

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
  let color =
    match (Sys.getenv_opt "NO_COLOR", kind) with
    | (None | Some ""), `warning -> `Yellow
    | (None | Some ""), `error -> `Red
    | _ -> `None
  in

  let header =
    match kind with
    | `warning -> "WARNING"
    | `error -> "ERROR"
  in
  Fmt.pf ppf "@[%a@] " (print_header ~color) header;
  Fmt.pf ppf "@[%a@]@," print_loc loc;

  let source_code = Source.content loc.loc_start.source in
  if source_code <> "" then
    Fmt.pf ppf "@,%s" (print_code ~color ~loc source_code)
;;
