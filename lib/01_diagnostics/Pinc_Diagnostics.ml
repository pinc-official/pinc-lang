module Location = Location
module Source = Pinc_Source

exception Pinc_error of string

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

  let () =
    lines
    |> List.iter @@ fun (line_number, line) ->
       if line_number >= highlight_line_start && line_number <= highlight_line_end then (
         let num_str = Printf.sprintf "%4d" line_number in
         Format.fprintf ppf "%s" (Style.wrap ~bold:true ?color num_str))
       else
         Format.fprintf ppf "%4d" line_number;

       Format.fprintf ppf " %s " (Style.faint "│");

       let did_highlight = ref false in

       let should_highlight column_number =
         let on_line_start = Int.equal line_number highlight_line_start in
         let after_column_start = column_number >= highlight_column_start in
         let between_lines =
           line_number >= highlight_line_start && line_number <= highlight_line_end
         in
         let between_columns =
           column_number >= highlight_column_start && column_number < highlight_column_end
         in
         let on_line_end = Int.equal line_number highlight_line_end in
         let before_column_end = column_number < highlight_column_end in

         (on_line_start && (not on_line_end) && after_column_start)
         || (between_lines && between_columns)
         || (on_line_end && (not on_line_start) && before_column_end)
       in

       let () =
         line
         |> String.iteri @@ fun column_index ch ->
            let column_number = column_index + 1 in
            if should_highlight column_number then (
              let ch_str = String.make 1 ch in
              Format.fprintf ppf "%s" (Style.wrap ~bold:true ?color ch_str);
              did_highlight := true)
            else
              Format.fprintf ppf "%c" ch
       in

       let () =
         if !did_highlight && color = None then (
           Format.pp_print_newline ppf ();
           Format.fprintf ppf "     %s " (Style.faint "│");
           line
           |> String.iteri @@ fun column_index _ch ->
              if should_highlight (succ column_index) then
                Format.fprintf ppf "^"
              else
                Format.fprintf ppf " ")
       in

       Format.pp_print_newline ppf ()
  in

  Buffer.contents buf
;;

let print_header ppf ~color text =
  Format.fprintf ppf "%s" (Style.wrap ~bold:true ?color text)
;;

let print ~kind ppf (loc : Location.t) =
  let color =
    match (Sys.getenv_opt "NO_COLOR", kind) with
    | (None | Some ""), `warning -> Some `Yellow
    | (None | Some ""), `error -> Some `Red
    | _ -> None
  in

  let header =
    match kind with
    | `warning -> "WARNING"
    | `error -> "ERROR"
  in
  Format.fprintf ppf "@[%a@] " (print_header ~color) header;
  Format.fprintf ppf "@[%a@]@," Location.pp loc;

  let source_code = loc |> Location.get_source |> Source.content in
  if source_code <> "" then
    Format.fprintf ppf "@,%s" (print_code ~color ~loc source_code)
;;

let print_error location message =
  let ppf = Format.err_formatter in
  Format.fprintf ppf "@[<v>@,%a@,%s@,@]" (print ~kind:`error) location message
;;

let raise_error location message =
  print_error location message;

  let buf = Buffer.create 1 in
  let ppf = Format.formatter_of_buffer buf in
  Format.fprintf ppf "@[<v>@,%a@,%s@,@]" (print ~kind:`error) location message;
  raise (Pinc_error (Buffer.contents buf))
;;

let warn location message =
  let ppf = Format.err_formatter in
  Format.fprintf ppf "@[<v>@,%a@,%s@,@]" (print ~kind:`warning) location message
;;

let flush () =
  let ppf = Format.err_formatter in
  Format.fprintf ppf "%!"
;;
