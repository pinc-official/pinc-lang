type typ =
  | NonTerminatedString
  | NonTerminatedTemplate
  | UnknownCharacter      of char
  | UnexpectedToken       of Token.token_type
  | ExpectedUpperIdent    of Token.token_type
  | ExpectedLowerIdent    of Token.token_type
  | ExpectedToken         of Token.token_type
  | Message               of string

type t = {
  start_pos: Position.t;
  end_pos: Position.t;
  typ: typ
}

let make ~start_pos ~end_pos typ = { start_pos; end_pos; typ }

module Messages = struct
  let non_terminated_string = "This string is not terminated. Please add a double-quote (\") at the end."

  let non_terminated_template = "Your Template was not closed correctly. You probably mismatched or forgot a closing tag."

  let unknown_character = Printf.sprintf "The character %C is unknown. You should remove it."

  let unexpected_token t = Printf.sprintf "The token %S is unexpected at this point." (Token.to_string t)

  let expected_uppercase_ident = begin function
  | Token.IDENT_LOWER i -> Printf.sprintf "Expected to see an uppercase identifier at this point. Did you mean %s instead of %s?" (String.capitalize_ascii i) i
  | _                   -> "Expected to see an uppercase identifier at this point."
  end

  let expected_lowercase_ident = begin function
  | Token.IDENT_UPPER i       -> Printf.sprintf "Expected to see a lowercase identifier at this point. Did you mean %s instead of %s?" (String.lowercase_ascii i) i
  | t when Token.is_keyword t -> Printf.sprintf "`%s` is a keyword. Please choose another name." (Token.to_string t)
  | _                         -> "Expected to see a lowercase identifier at this point."
  end

  let expected_token t = Printf.sprintf "Expected token %s at this point." (Token.to_string t)

  let make t = begin match t.typ with
    | NonTerminatedString   -> non_terminated_string
    | NonTerminatedTemplate -> non_terminated_template
    | UnknownCharacter c    -> unknown_character c
    | UnexpectedToken t     -> unexpected_token t
    | ExpectedUpperIdent t  -> expected_uppercase_ident t
    | ExpectedLowerIdent t  -> expected_lowercase_ident t
    | ExpectedToken t       -> expected_token t
    | Message m             -> m
  end
end

module Reporter = struct
  let print fmt ~position message = begin
    let print_pos fmt positions = begin
      Position.(
        let dim_loc fmt (start_pos, end_pos) = begin
          if start_pos.Position.line = end_pos.line then
            if start_pos.column = end_pos.column then
              Format.fprintf fmt "at @{<dim>line %i, column %i@}" start_pos.line start_pos.column
            else
              Format.fprintf fmt "at @{<dim>line %i@} from @{<dim>column %i to %i@}" start_pos.line start_pos.column end_pos.column
          else
            Format.fprintf fmt "from @{<dim>line %i, column %i@} to @{<dim>line %i, column %i@}" start_pos.line start_pos.column end_pos.line end_pos.column
        end in
        Format.fprintf fmt "@{<filename>%s@} %a" ((fst positions).filename) dim_loc positions
      )
    end in

    Format.fprintf fmt "  @[%a@]@," print_pos position;
    Format.fprintf fmt "@[<v>@,  %s@,@]" message;
  end

  let print t = begin
    Format.fprintf Format.err_formatter "@[<v>";

    let position = (t.start_pos, t.end_pos) in
    let message = Messages.make t in
    print Format.err_formatter ~position message;

    Format.fprintf Format.err_formatter "@]@.";
  end
end

let report ~start_pos ~end_pos typ = begin
  Reporter.print {start_pos; end_pos; typ};
  exit 1
end
