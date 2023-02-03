module Position = Pinc_Position

module Reporter = struct
  let print ~position message =
    let print_pos positions =
      Position.(
        let get_loc (start_pos, end_pos) =
          let start_line = string_of_int start_pos.line in
          let start_col = string_of_int start_pos.column in
          let end_line = string_of_int end_pos.line in
          let end_col = string_of_int end_pos.column in
          if start_pos.Position.line = end_pos.line
          then
            if start_pos.column = end_pos.column
            then "at line " ^ start_line ^ ", column " ^ start_col
            else "at line " ^ start_line ^ " from column " ^ start_col ^ " to " ^ end_col
          else
            "from line "
            ^ start_line
            ^ ", column "
            ^ start_col
            ^ " to line "
            ^ end_line
            ^ ", column "
            ^ end_col
        in
        (fst positions).filename ^ " " ^ get_loc positions)
    in
    prerr_endline (print_pos position);
    prerr_endline message
  ;;
end

let report ~start_pos ~end_pos message =
  let position = start_pos, end_pos in
  Reporter.print ~position message;
  exit 1
;;
