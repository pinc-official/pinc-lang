module Position = Pinc_Position
module Token = Pinc_Token

type typ =
  | NonTerminatedString
  | NonTerminatedTemplate
  | UnknownCharacter of char
  | UnexpectedToken of Token.token_type
  | ExpectedIdent of Token.token_type
  | ExpectedUpperIdent of Token.token_type
  | ExpectedLowerIdent of Token.token_type
  | ExpectedToken of Token.token_type
  | Message of string

type t = {start_pos: Position.t; end_pos: Position.t; typ: typ}

let make ~start_pos ~end_pos typ = {start_pos; end_pos; typ}

module Messages = struct
  let non_terminated_string =
    "This string is not terminated. Please add a double-quote (\") at the end."

  let non_terminated_template =
    "Your Template was not closed correctly. You probably mismatched or forgot \
     a closing tag."

  let unknown_character c =
    "The character " ^ String.make 1 c ^ " is unknown. You should remove it."

  let unexpected_token t =
    "The token " ^ Token.to_string t ^ " is unexpected at this point."

  let expected_ident = function
    | t when Token.is_keyword t ->
        "\"" ^ Token.to_string t
        ^ "\" is a keyword. Please choose another name."
    | _ ->
        "Expected to see an identifier at this point."

  let expected_uppercase_ident = function
    | Token.IDENT_LOWER i ->
        "Expected to see an uppercase identifier at this point. Did you mean "
        ^ String.capitalize_ascii i ^ " instead of " ^ i ^ "?"
    | _ ->
        "Expected to see an uppercase identifier at this point."

  let expected_lowercase_ident = function
    | Token.IDENT_UPPER i ->
        "Expected to see a lowercase identifier at this point. Did you mean "
        ^ String.lowercase_ascii i ^ " instead of " ^ i ^ "?"
    | t when Token.is_keyword t ->
        "`" ^ Token.to_string t ^ "` is a keyword. Please choose another name."
    | t ->
        "Expected to see a lowercase identifier at this point. Instead saw "
        ^ Token.to_string t

  let expected_token t =
    "Expected token " ^ Token.to_string t ^ " at this point."

  let make t =
    match t.typ with
    | NonTerminatedString ->
        non_terminated_string
    | NonTerminatedTemplate ->
        non_terminated_template
    | UnknownCharacter c ->
        unknown_character c
    | UnexpectedToken t ->
        unexpected_token t
    | ExpectedIdent t ->
        expected_ident t
    | ExpectedUpperIdent t ->
        expected_uppercase_ident t
    | ExpectedLowerIdent t ->
        expected_lowercase_ident t
    | ExpectedToken t ->
        expected_token t
    | Message m ->
        m
end

module Reporter = struct
  let print ~position message =
    let print_pos positions =
      Position.(
        let get_loc (start_pos, end_pos) =
          let start_line = string_of_int start_pos.line in
          let start_col = string_of_int start_pos.column in
          let end_line = string_of_int end_pos.line in
          let end_col = string_of_int end_pos.column in
          if start_pos.Position.line = end_pos.line then
            if start_pos.column = end_pos.column then
              "at line " ^ start_line ^ ", column " ^ start_col
            else
              "at line " ^ start_line ^ " from column " ^ start_col ^ " to "
              ^ end_col
          else
            "from line " ^ start_line ^ ", column " ^ start_col ^ " to line "
            ^ end_line ^ ", column " ^ end_col
        in
        (fst positions).filename ^ " " ^ get_loc positions)
    in
    prerr_endline (print_pos position) ;
    prerr_endline message

  let print t =
    let position = (t.start_pos, t.end_pos) in
    let message = Messages.make t in
    print ~position message
end

let report ~start_pos ~end_pos typ =
  Reporter.print {start_pos; end_pos; typ} ;
  exit 1
