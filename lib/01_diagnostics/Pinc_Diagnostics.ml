module Location = Location
module Source = Pinc_Source

exception Pinc_error

let print_code ~color ~loc source_code =
  let context_lines = 1 in
  let start_pos = loc |> Location.get_start in
  let end_pos = loc |> Location.get_end in
  let highlight_line_start = start_pos |> Location.Position.get_line in
  let highlight_line_end = end_pos |> Location.Position.get_line in
  let highlight_column_start = start_pos |> Location.Position.get_column in
  let highlight_column_end = end_pos |> Location.Position.get_column in

  let first_shown_line = highlight_line_start - context_lines |> Int.max 0 in
  let last_shown_line = highlight_line_end + context_lines in

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
  Fmt.pf ppf "@[%a@]@," Location.pp loc;

  let source_code = loc |> Location.get_source |> Source.content in
  if source_code <> "" then
    Fmt.pf ppf "@,%s" (print_code ~color ~loc source_code)
;;

let set_renderer ppf =
  match Sys.getenv_opt "NO_COLOR" with
  | None | Some "" -> Fmt.set_style_renderer ppf `Ansi_tty
  | Some _ -> Fmt.set_style_renderer ppf `None
;;

let print_error location message =
  let ppf = Format.err_formatter in
  set_renderer ppf;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (print ~kind:`error) location message
;;

let raise_error location message =
  print_error location message;
  raise Pinc_error
;;

let warn location message =
  let ppf = Format.err_formatter in
  set_renderer ppf;
  Fmt.pf ppf "@[<v>@,%a@,%s@,@]" (print ~kind:`warning) location message
;;
