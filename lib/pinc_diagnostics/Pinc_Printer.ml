(*
  This file is taken from ReScript's super_code_frame.ml and super_location.ml
  We're copying the look of ReScript's terminal error reporting.
  See https://github.com/rescript-lang/syntax/pull/77 for the rationale.
  A few lines have been commented out and swapped for their tweaked version.
*)

module Location = Pinc_Location
module Position = Pinc_Position

(* ===== super_code_frame.ml *)

module Super_code_frame = struct
  let seek_2_lines_before src pos =
    let original_line = pos.Position.line in
    let rec loop current_line current_char =
      if current_line + 2 >= original_line
      then current_char, current_line
      else
        loop
          (if src.[current_char] = '\n' then current_line + 1 else current_line)
          (current_char + 1)
    in
    loop 1 0
  ;;

  let seek_2_lines_after src pos =
    let original_line = pos.Position.line in
    let rec loop current_line current_char =
      if current_line - 3 >= original_line
      then current_char - 1, current_line
      else
        loop
          (if src.[current_char] = '\n' then current_line + 1 else current_line)
          (current_char + 1)
    in
    loop 1 0
  ;;

  let leading_space_count str =
    let rec loop i count =
      if i = String.length str
      then count
      else if str.[i] != ' '
      then count
      else loop (i + 1) (count + 1)
    in
    loop 0 0
  ;;

  let break_long_line max_width line =
    let rec loop pos accum =
      if pos = String.length line
      then accum
      else (
        let chunk_length = min max_width (String.length line - pos) in
        let chunk = String.sub line pos chunk_length in
        loop (pos + chunk_length) (chunk :: accum))
    in
    loop 0 [] |> List.rev
  ;;

  let filter_mapi f l =
    let rec loop f l i accum =
      match l with
      | [] -> accum
      | head :: rest ->
        let accum =
          match f i head with
          | None -> accum
          | Some result -> result :: accum
        in
        loop f rest (i + 1) accum
    in
    loop f l 0 [] |> List.rev
  ;;

  (* Spiritual equivalent of
     https://github.com/ocaml/ocaml/blob/414bdec9ae387129b8102cc6bf3c0b6ae173eeb9/utils/misc.ml#L601
  *)
  module Color = struct
    type color =
      | Dim
      (* | Filename *)
      | Err
      | Warn
      | NoColor

    let dim = "\x1b[2m"

    (* let filename = "\x1b[46m" *)
    let err = "\x1b[1;31m"
    let warn = "\x1b[1;33m"
    let reset = "\x1b[0m"

    external isatty : out_channel -> bool = "caml_sys_isatty"

    (* reasonable heuristic on whether colors should be enabled *)
    let should_enable_color () =
      let term =
        try Sys.getenv "TERM" with
        | Not_found -> ""
      in
      term <> "dumb" && term <> "" && isatty stderr
    ;;
  end

  type gutter =
    | Number of int
    | Elided

  type highlighted_string =
    { s : string
    ; start : int
    ; end_ : int
    }

  type line =
    { gutter : gutter
    ; content : highlighted_string list
    }

  (*
  Features:
  - display a line gutter
  - break long line into multiple for terminal display
  - peek 2 lines before & after for context
  - center snippet when it's heavily indented
  - ellide intermediate lines when the reported range is huge
*)
  let print ~is_warning ~src ~startPos ~endPos =
    let indent = 2 in
    let highlight_line_start_line = startPos.Position.line in
    let highlight_line_end_line = endPos.Position.line in
    let start_line_line_offset, first_shown_line = seek_2_lines_before src startPos in
    let end_line_line_offset, last_shown_line = seek_2_lines_after src endPos in
    let more_than_5_highlighted_lines =
      highlight_line_end_line - highlight_line_start_line + 1 > 5
    in
    let line_width = 100 in
    let lines =
      String.sub src start_line_line_offset (end_line_line_offset - start_line_line_offset)
      |> String.split_on_char '\n'
      |> filter_mapi (fun i line ->
           let line_number = i + first_shown_line in
           if more_than_5_highlighted_lines
           then
             if line_number = highlight_line_start_line + 2
             then Some (Elided, line)
             else if line_number > highlight_line_start_line + 2
                     && line_number < highlight_line_end_line - 1
             then None
             else Some (Number line_number, line)
           else Some (Number line_number, line))
    in
    let leading_space_to_cut =
      lines
      |> List.fold_left
           (fun current_max (_, line) ->
             let leading_spaces = leading_space_count line in
             if String.length line = leading_spaces
             then (* the line's nothing but spaces. Doesn't count *)
               current_max
             else min leading_spaces current_max)
           Int.max_int
    in
    let separator = if leading_space_to_cut = 0 then "│" else "┆" in
    let stripped_lines =
      lines
      |> List.map (fun (gutter, line) ->
           let new_content =
             if String.length line <= leading_space_to_cut
             then [ { s = ""; start = 0; end_ = 0 } ]
             else
               String.sub
                 line
                 leading_space_to_cut
                 (String.length line - leading_space_to_cut)
               |> break_long_line line_width
               |> List.mapi (fun i line ->
                    match gutter with
                    | Elided -> { s = line; start = 0; end_ = 0 }
                    | Number line_number ->
                      let highlight_line_start_offset =
                        startPos.Position.column - startPos.Position.beginning_of_line
                      in
                      let highlight_line_end_offset =
                        endPos.Position.column - endPos.Position.beginning_of_line
                      in
                      let start =
                        if i = 0 && line_number = highlight_line_start_line
                        then highlight_line_start_offset - leading_space_to_cut
                        else 0
                      in
                      let end_ =
                        if line_number < highlight_line_start_line
                        then 0
                        else if line_number = highlight_line_start_line
                                && line_number = highlight_line_end_line
                        then highlight_line_end_offset - leading_space_to_cut
                        else if line_number = highlight_line_start_line
                        then String.length line
                        else if line_number > highlight_line_start_line
                                && line_number < highlight_line_end_line
                        then String.length line
                        else if line_number = highlight_line_end_line
                        then highlight_line_end_offset - leading_space_to_cut
                        else 0
                      in
                      { s = line; start; end_ })
           in
           { gutter; content = new_content })
    in
    let buf = Buffer.create 100 in
    let open Color in
    let add_ch =
      let last_color = ref NoColor in
      fun color ch ->
        if !last_color = color
        then Buffer.add_char buf ch
        else (
          let ansi =
            match !last_color, color with
            | NoColor, Dim -> dim
            (* | NoColor, Filename -> filename *)
            | NoColor, Err -> err
            | NoColor, Warn -> warn
            | _, NoColor -> reset
            | _, Dim -> reset ^ dim
            (* | _, Filename -> reset ^ filename *)
            | _, Err -> reset ^ err
            | _, Warn -> reset ^ warn
          in
          Buffer.add_string buf ansi;
          Buffer.add_char buf ch;
          last_color := color)
    in
    let draw_gutter color s =
      for _i = 1 to last_shown_line + indent - String.length s do
        add_ch NoColor ' '
      done;
      s |> String.iter (add_ch color);
      add_ch NoColor ' ';
      separator |> String.iter (add_ch Dim);
      add_ch NoColor ' '
    in
    stripped_lines
    |> List.iter (fun { gutter; content } ->
         match gutter with
         | Elided ->
           draw_gutter Dim ".";
           add_ch Dim '.';
           add_ch Dim '.';
           add_ch Dim '.';
           add_ch NoColor '\n'
         | Number line_number ->
           content
           |> List.iteri (fun i line ->
                let gutter_content = if i = 0 then string_of_int line_number else "" in
                let gutter_color =
                  if i = 0
                     && line_number >= highlight_line_start_line
                     && line_number <= highlight_line_end_line
                  then if is_warning then Warn else Err
                  else NoColor
                in
                draw_gutter gutter_color gutter_content;
                line.s
                |> String.iteri (fun ii ch ->
                     let c =
                       if ii >= line.start && ii < line.end_
                       then if is_warning then Warn else Err
                       else NoColor
                     in
                     add_ch c ch);
                add_ch NoColor '\n'));
    Buffer.contents buf
  ;;
end

(* ===== super_location.ml *)
module Super_location = struct
  let print_filename ppf file = Format.fprintf ppf "%s" file

  let print_loc ~range ppf (loc : Location.t) =
    let dim_loc ppf = function
      | (start_line, start_line_start_char), (end_line, end_line_end_char) ->
        if start_line = end_line
        then
          if start_line_start_char = end_line_end_char
          then Format.fprintf ppf ":@{<dim>%i:%i@}" start_line start_line_start_char
          else
            Format.fprintf
              ppf
              ":@{<dim>%i:%i-%i@}"
              start_line
              start_line_start_char
              end_line_end_char
        else
          Format.fprintf
            ppf
            ":@{<dim>%i:%i-%i:%i@}"
            start_line
            start_line_start_char
            end_line
            end_line_end_char
    in
    Format.fprintf
      ppf
      "@{<filename>%a@}%a"
      print_filename
      loc.loc_start.filename
      dim_loc
      range
  ;;

  (* let print ~message_kind intro ppf (loc : Location.t) = *)
  let print ~message_kind src ppf (loc : Location.t) =
    (match message_kind with
     | `warning -> Format.fprintf ppf "@[@{<info>%s@}@]@," "Warning!"
     | `error -> Format.fprintf ppf "@[@{<error>%s@}@]@," "Error!");
    (* ocaml's reported line/col numbering is horrible and super error-prone
       when being handled programmatically (or humanly for that matter. If you're
       an ocaml contributor reading this: who the heck reads the character count
       starting from the first erroring character?) *)
    (* let (file, start_line, start_char) = Location.get_pos_info loc.loc_start in *)
    let start_line = loc.loc_start.line in
    let start_char = loc.loc_start.column - loc.loc_start.beginning_of_line in
    let end_line = loc.loc_end.line in
    let end_char = loc.loc_end.column - loc.loc_end.beginning_of_line in
    (* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion *)
    (* start_char is inclusive, end_char is exclusive *)
    let range = (start_line, start_char), (end_line, end_char) in
    Format.fprintf ppf "  @[%a@]@," (print_loc ~range) loc;
    try
      (* let src = Ext_io.load_file file in *)
      (* we're putting the line break `@,` here rather than above, because this
           branch might not be reached (aka no inline file content display) so
           we don't wanna end up with two line breaks in the the consequent *)
      Format.fprintf
        ppf
        "@,%s"
        (Super_code_frame.print
           ~is_warning:(message_kind = `warning)
           ~src
           ~startPos:loc.loc_start
           ~endPos:loc.loc_end)
    with
    (* this might happen if the file is e.g. "", "_none_" or any of the fake file name placeholders.
         we've already printed the location above, so nothing more to do here. *)
    | Sys_error _ -> ()
  ;;

  (* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L380 *)
  (* This is the error report entry point. We'll replace the default reporter with this one. *)
  (* let rec super_error_reporter ppf ({loc; msg; sub} : Location.error) = *)
  let super_error_reporter ppf src loc msg =
    (* open a vertical box. Everything in our message is indented 2 spaces *)
    (* Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]" (print ~message_kind:`error "We've found a bug for you!") src loc msg; *)
    Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]" (print ~message_kind:`error src) loc msg
  ;;
  (* List.iter (Format.fprintf ppf "@,@[%a@]" super_error_reporter) sub *)
  (* no need to flush here; location's report_exception (which uses this ultimately) flushes *)
end
