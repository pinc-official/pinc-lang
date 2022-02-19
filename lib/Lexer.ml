type mode =
  | Normal
  | TemplateAttributes
  | ComponentAttributes
  | Template

type char' =
  [ `Chr of char
  | `EOF
  ]

type t =
  { filename : string
  ; src : string
  ; src_length : int
  ; mutable prev : char'
  ; mutable current : char'
  ; mutable offset : int
  ; mutable line_offset : int
  ; mutable line : int
  ; mutable column : int
  ; mutable mode : mode list
  }

let make_position a =
  Position.make ~filename:a.filename ~line:a.line ~column:(a.column + 1)
;;

let show_mode = function
  | Normal -> "NORMAL"
  | TemplateAttributes -> "TemplateAttributes"
  | ComponentAttributes -> "ComponentAttributes"
  | Template -> "Template"
;;

let inNormalMode t =
  match t.mode with
  | Normal :: _ -> true
  | _ -> false
;;

let setNormalMode t = t.mode <- Normal :: t.mode

let popNormalMode t =
  match t.mode with
  | Normal :: tl -> t.mode <- tl
  | _ -> ()
;;

let inTemplateMode t =
  match t.mode with
  | Template :: _ -> true
  | _ -> false
;;

let setTemplateMode t = t.mode <- Template :: t.mode

let popTemplateMode t =
  match t.mode with
  | Template :: tl -> t.mode <- tl
  | _ -> ()
;;

let inTemplateAttributesMode t =
  match t.mode with
  | TemplateAttributes :: _ -> true
  | _ -> false
;;

let setTemplateAttributesMode t = t.mode <- TemplateAttributes :: t.mode

let popTemplateAttributesMode t =
  match t.mode with
  | TemplateAttributes :: tl -> t.mode <- tl
  | _ -> ()
;;

let inComponentAttributesMode t =
  match t.mode with
  | ComponentAttributes :: _ -> true
  | _ -> false
;;

let setComponentAttributesMode t = t.mode <- ComponentAttributes :: t.mode

let popComponentAttributesMode t =
  match t.mode with
  | ComponentAttributes :: tl -> t.mode <- tl
  | _ -> ()
;;

let next t =
  let next_offset = t.offset + 1 in
  let () =
    match t.current with
    | `Chr '\n' ->
      t.line_offset <- next_offset;
      t.line <- succ t.line
    | _ -> ()
  in
  t.column <- next_offset - t.line_offset;
  t.offset <- next_offset;
  t.prev <- t.current;
  t.current
    <- (if next_offset < t.src_length
       then `Chr (String.unsafe_get t.src next_offset)
       else `EOF)
;;

let next_n ~n t =
  for _ = 1 to n do
    next t
  done
;;

let peek ?(n = 1) t =
  if t.offset + n < t.src_length
  then `Chr (String.unsafe_get t.src (t.offset + n))
  else `EOF
;;

let make ~filename src =
  { filename
  ; src
  ; src_length = String.length src
  ; prev = `EOF
  ; current = (if src = "" then `EOF else `Chr (String.unsafe_get src 0))
  ; offset = 0
  ; line_offset = 0
  ; column = 0
  ; line = 1
  ; mode = [ Normal ]
  }
;;

let is_whitespace = function
  | `Chr ' ' | `Chr '\t' | `Chr '\n' | `Chr '\r' -> true
  | _ -> false
;;

let rec skip_whitespace t =
  if is_whitespace t.current
  then (
    next t;
    skip_whitespace t)
;;

let scan_ident t =
  let buf = Buffer.create 32 in
  (* NOTE: List all vaid chars for identifiers here: *)
  let rec loop t =
    match t.current with
    | `Chr ('A' .. 'Z' as c)
    | `Chr ('a' .. 'z' as c)
    | `Chr ('0' .. '9' as c)
    | `Chr ('_' as c) ->
      next t;
      Buffer.add_char buf c;
      loop t
    | _ -> ()
  in
  let () = loop t in
  Token.lookup_keyword (Buffer.contents buf)
;;

let scan_string t =
  let start_pos = make_position t in
  let buf = Buffer.create 512 in
  let rec loop t =
    match t.current with
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedString
    | `Chr '\\' ->
      (match peek t with
      | `Chr ' ' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\\';
        loop t
      | `Chr '"' ->
        next_n ~n:2 t;
        Buffer.add_char buf '"';
        loop t
      | `Chr '\'' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\'';
        loop t
      | `Chr 'b' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\b';
        loop t
      | `Chr 'f' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\012';
        loop t
      | `Chr 'n' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\n';
        loop t
      | `Chr 'r' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\r';
        loop t
      | `Chr 't' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\t';
        loop t
      | `Chr '\\' ->
        next_n ~n:2 t;
        Buffer.add_char buf '\\';
        loop t
      | `Chr _ ->
        Diagnostics.report
          ~start_pos:(make_position t)
          ~end_pos:(make_position t)
          (Diagnostics.Message "Unknown escape sequence in string.")
      | `EOF ->
        Diagnostics.report
          ~start_pos
          ~end_pos:(make_position t)
          Diagnostics.NonTerminatedString)
    | `Chr '"' ->
      next t;
      ()
    | `Chr c ->
      next t;
      Buffer.add_char buf c;
      loop t
  in
  (* NEXT once to skip the beginning double quote *)
  next t;
  let () = loop t in
  Token.STRING (Buffer.contents buf)
;;

let scan_tag_or_template t =
  let start_pos = make_position t in
  let buf = Buffer.create 32 in
  (* NOTE: List all vaid chars for identifiers here: *)
  let rec loop t =
    match t.current with
    | `Chr ('A' .. 'Z' as c)
    | `Chr ('a' .. 'z' as c)
    | `Chr ('0' .. '9' as c)
    | `Chr ('_' as c) ->
      next t;
      Buffer.add_char buf c;
      loop t
    | _ -> ()
  in
  (* NEXT once to skip the beginning # symbol *)
  next t;
  let () = loop t in
  let found = Buffer.contents buf in
  match found with
  | "Template" -> Token.TEMPLATE
  | s ->
    (match s.[0] with
    | 'A' .. 'Z' -> Token.TAG found
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.Message
           (Printf.sprintf
              "Invalid Tag! Tags have to start with an uppercase character. Did you mean \
               to write #%s?"
              (String.capitalize_ascii found))))
;;

let get_html_tag_ident t =
  let start_pos = make_position t in
  let rec loop buf t =
    match t.current with
    | `Chr ('a' .. 'z' as c) | `Chr ('0' .. '9' as c) | `Chr ('-' as c) ->
      next t;
      Buffer.add_char buf c;
      loop buf t
    | _ -> Buffer.contents buf
  in
  let buf = Buffer.create 32 in
  let ident = loop buf t in
  skip_whitespace t;
  match ident.[0] with
  | 'a' .. 'z' -> ident
  | _ ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      (Diagnostics.Message
         (Printf.sprintf
            "Invalid HTML tag! HTML tags have to start with a lowercase letter. Instead \
             saw: %s"
            ident))
;;

let get_uppercase_ident t =
  (* NOTE: List all vaid chars for identifiers here: *)
  let rec loop buf t =
    match t.current with
    | `Chr ('A' .. 'Z' as c)
    | `Chr ('a' .. 'z' as c)
    | `Chr ('0' .. '9' as c)
    | `Chr ('_' as c) ->
      next t;
      Buffer.add_char buf c;
      loop buf t
    | _ -> Buffer.contents buf
  in
  let buf = Buffer.create 32 in
  loop buf t
;;

let scan_open_tag t =
  let start_pos = make_position t in
  (* NEXT once to skip the beginning < char *)
  next t;
  match t.current with
  | `Chr 'a' .. 'z' -> Token.HTML_OPEN_TAG (get_html_tag_ident t)
  | `Chr 'A' .. 'Z' -> Token.COMPONENT_OPEN_TAG (get_uppercase_ident t)
  | `Chr c ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      (Diagnostics.Message
         (Printf.sprintf
            "Invalid Template tag! Template tags have to start with an uppercase or \
             lowercase letter. Instead saw: %c"
            c))
  | `EOF ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      Diagnostics.NonTerminatedTemplate
;;

let scan_close_tag t =
  let start_pos = make_position t in
  (* NEXT twice to skip the beginning </ chars *)
  next_n ~n:2 t;
  let close_tag =
    match t.current with
    | `Chr 'a' .. 'z' -> Token.HTML_CLOSE_TAG (get_html_tag_ident t)
    | `Chr 'A' .. 'Z' -> Token.COMPONENT_CLOSE_TAG (get_uppercase_ident t)
    | `Chr c ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.Message
           (Printf.sprintf
              "Invalid Template tag! Template tags have to start with an uppercase or \
               lowercase letter. Instead saw: %c"
              c))
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedTemplate
  in
  match t.current with
  | `Chr '>' ->
    next t;
    close_tag
  | _ ->
    Diagnostics.report ~start_pos ~end_pos:(make_position t) (ExpectedToken Token.GREATER)
;;

let scan_template_text t =
  let start_pos = make_position t in
  let rec loop buf t =
    match t.current with
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedTemplate
    | `Chr '{' -> Buffer.contents buf
    | `Chr '<' ->
      (match peek t with
      | `Chr '/' -> Buffer.contents buf
      | `Chr 'a' .. 'z' | `Chr 'A' .. 'Z' -> Buffer.contents buf
      | _ ->
        next t;
        Buffer.add_char buf '<';
        loop buf t)
    | `Chr c ->
      next t;
      Buffer.add_char buf c;
      loop buf t
  in
  let buf = Buffer.create 32 in
  let found = loop buf t in
  Token.STRING found
;;

let scan_html_attribute_ident t =
  (* NOTE: List all vaid chars for html identifiers here: *)
  let rec loop buf t =
    match t.current with
    | `Chr ('A' .. 'Z' as c)
    | `Chr ('a' .. 'z' as c)
    | `Chr ('0' .. '9' as c)
    | `Chr (':' as c)
    | `Chr ('-' as c) ->
      next t;
      Buffer.add_char buf c;
      loop buf t
    | _ -> Buffer.contents buf
  in
  let buf = Buffer.create 32 in
  let found = loop buf t in
  Token.lookup_keyword found
;;

let scan_number t =
  let result = Buffer.create 5 in
  let rec scan_digits t =
    match t.current with
    | `Chr ('0' .. '9' as c) ->
      Buffer.add_char result c;
      next t;
      scan_digits t
    | `Chr '_' ->
      next t;
      scan_digits t
    | _ -> ()
  in
  scan_digits t;
  let is_float =
    match t.current with
    | `Chr '.' ->
      (match peek t with
      | `Chr '.' -> false (* Two Dots in a row mean that this is a range operator *)
      | _ ->
        Buffer.add_char result '.';
        next t;
        scan_digits t;
        true)
    | _ -> false
  in
  (* exponent part *)
  let is_float =
    match t.current with
    | `Chr 'e' | `Chr 'E' ->
      Buffer.add_char result 'e';
      next t;
      let () =
        match t.current with
        | `Chr '+' ->
          Buffer.add_char result '+';
          next t
        | `Chr '-' ->
          Buffer.add_char result '-';
          next t
        | _ -> ()
      in
      scan_digits t;
      true
    | _ -> is_float
  in
  let result = Buffer.contents result in
  if is_float
  then Token.FLOAT (float_of_string result)
  else Token.INT (int_of_string result)
;;

let skip_comment t =
  let rec skip t =
    match t.current with
    | `Chr '\n' | `Chr '\r' -> ()
    | `EOF -> ()
    | _ ->
      next t;
      skip t
  in
  skip t
;;

let rec scan_template_token ~start_pos t =
  match t.current with
  | `Chr '{' ->
    setNormalMode t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popNormalMode t;
    next t;
    Token.RIGHT_BRACE
  | `Chr '<' ->
    (match peek t with
    | `Chr 'a' .. 'z' ->
      setTemplateMode t;
      setTemplateAttributesMode t;
      scan_open_tag t
    | `Chr 'A' .. 'Z' ->
      setTemplateMode t;
      setComponentAttributesMode t;
      scan_open_tag t
    | `Chr '/' ->
      popTemplateMode t;
      scan_close_tag t
    | _ -> scan_template_text t)
  | `Chr '/' ->
    (match peek t with
    | `Chr '>' ->
      popTemplateMode t;
      next_n ~n:2 t;
      Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING
    | `Chr c ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter c)
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedTemplate)
  | `Chr '>' ->
    next t;
    Token.HTML_OR_COMPONENT_TAG_END
  | `EOF ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      Diagnostics.NonTerminatedTemplate
  | _ -> scan_template_text t

and scan_component_attributes_token ~start_pos t =
  match t.current with
  | `Chr '_' | `Chr 'a' .. 'z' -> scan_ident t
  | `Chr '"' -> scan_string t
  | `Chr '=' ->
    next t;
    Token.EQUAL
  | `Chr '{' ->
    setNormalMode t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popNormalMode t;
    next t;
    Token.RIGHT_BRACE
  | `Chr '/' ->
    (match peek t with
    | `Chr '>' ->
      popComponentAttributesMode t;
      scan_template_token ~start_pos t
    | `Chr c ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter c)
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedTemplate)
  | `Chr '>' ->
    popComponentAttributesMode t;
    scan_template_token ~start_pos t
  | `EOF ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      Diagnostics.NonTerminatedTemplate
  | _ -> scan_template_text t

and scan_template_attributes_token ~start_pos t =
  match t.current with
  | `Chr 'A' .. 'Z' | `Chr 'a' .. 'z' -> scan_html_attribute_ident t
  | `Chr '"' -> scan_string t
  | `Chr '=' ->
    next t;
    Token.EQUAL
  | `Chr '{' ->
    setNormalMode t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popNormalMode t;
    next t;
    Token.RIGHT_BRACE
  | `Chr '/' ->
    (match peek t with
    | `Chr '>' ->
      popTemplateAttributesMode t;
      scan_template_token ~start_pos t
    | `Chr c ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter c)
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedTemplate)
  | `Chr '>' ->
    popTemplateAttributesMode t;
    scan_template_token ~start_pos t
  | `EOF ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      Diagnostics.NonTerminatedTemplate
  | _ -> scan_template_text t

and scan_normal_token ~start_pos t =
  match t.current with
  | `Chr 'A' .. 'Z' | `Chr 'a' .. 'z' | `Chr '_' -> scan_ident t
  | `Chr '0' .. '9' -> scan_number t
  | `Chr '"' -> scan_string t
  | `Chr '(' ->
    next t;
    Token.LEFT_PAREN
  | `Chr ')' ->
    next t;
    Token.RIGHT_PAREN
  | `Chr '[' ->
    next t;
    Token.LEFT_BRACK
  | `Chr ']' ->
    next t;
    Token.RIGHT_BRACK
  | `Chr '{' ->
    (* NOTE: Do we always want to go into normal mode when seeing { ? *)
    setNormalMode t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popNormalMode t;
    next t;
    Token.RIGHT_BRACE
  | `Chr ':' ->
    (match peek t with
    | `Chr ':' ->
      next_n ~n:2 t;
      Token.DOUBLE_COLON
    | _ ->
      next t;
      Token.COLON)
  | `Chr ',' ->
    next t;
    Token.COMMA
  | `Chr ';' ->
    next t;
    Token.SEMICOLON
  | `Chr '-' ->
    if is_whitespace (peek t)
    then (
      next t;
      Token.MINUS)
    else (
      match peek t with
      | `Chr '>' ->
        next_n ~n:2 t;
        Token.ARROW
      | _ ->
        next t;
        Token.UNARY_MINUS)
  | `Chr '+' ->
    (match peek t with
    | `Chr '+' ->
      next_n ~n:2 t;
      Token.PLUSPLUS
    | _ ->
      next t;
      Token.PLUS)
  | `Chr '%' ->
    next t;
    Token.PERCENT
  | `Chr '?' ->
    next t;
    Token.QUESTIONMARK
  | `Chr '/' ->
    (match peek t with
    | `Chr '/' ->
      skip_comment t;
      Token.COMMENT
    | _ ->
      next t;
      Token.SLASH)
  | `Chr '.' ->
    (match peek t with
    | `Chr '0' .. '9' -> scan_number t
    | `Chr '.' ->
      (match peek ~n:2 t with
      | `Chr '.' ->
        next_n ~n:3 t;
        Token.DOTDOTDOT
      | _ ->
        next_n ~n:2 t;
        Token.DOTDOT)
    | _ ->
      next t;
      Token.DOT)
  | `Chr '@' ->
    (match peek t with
    | `Chr '@' ->
      next_n ~n:2 t;
      Token.ATAT
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter '@'))
  | `Chr '#' ->
    (match peek t with
    | `Chr 'A' .. 'Z' | `Chr 'a' .. 'z' -> scan_tag_or_template t
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter '#'))
  | `Chr '&' ->
    (match peek t with
    | `Chr '&' ->
      next_n ~n:2 t;
      Token.LOGICAL_AND
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter '&'))
  | `Chr '|' ->
    (match peek t with
    | `Chr '|' ->
      next_n ~n:2 t;
      Token.LOGICAL_OR
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter '|'))
  | `Chr '!' ->
    (match peek t with
    | `Chr '=' ->
      next_n ~n:2 t;
      Token.NOT_EQUAL
    | _ ->
      next t;
      Token.NOT)
  | `Chr '=' ->
    (match peek t with
    | `Chr '=' ->
      next_n ~n:2 t;
      Token.EQUAL_EQUAL
    | _ ->
      next t;
      Token.EQUAL)
  | `Chr '>' ->
    (match peek t with
    | `Chr '=' ->
      next_n ~n:2 t;
      Token.GREATER_EQUAL
    | _ ->
      next t;
      Token.GREATER)
  | `Chr '*' ->
    (match peek t with
    | `Chr '*' ->
      next_n ~n:2 t;
      Token.STAR_STAR
    | _ ->
      next t;
      Token.STAR)
  | `Chr '<' ->
    (match peek t with
    | `Chr '-' ->
      next_n ~n:2 t;
      Token.ARROW_LEFT
    | `Chr '=' ->
      next_n ~n:2 t;
      Token.LESS_EQUAL
    | `Chr '/' ->
      popTemplateMode t;
      scan_close_tag t
    | `Chr 'a' .. 'z' ->
      setTemplateMode t;
      setTemplateAttributesMode t;
      scan_open_tag t
    | `Chr 'A' .. 'Z' ->
      setTemplateMode t;
      setComponentAttributesMode t;
      scan_open_tag t
    | c when is_whitespace t.prev && is_whitespace c ->
      next t;
      Token.LESS
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter '<'))
  | `EOF -> Token.END_OF_INPUT
  | `Chr _ ->
    (* Diagnostics.report ~start_pos ~end_pos:(make_position t)
       (Diagnostics.UnknownCharacter c) *)
    (* NOTE: Maybe we can ignore this and continue scanning... *)
    next t;
    scan_token ~start_pos t

and scan_token ~start_pos t =
  (* Printf.printf "\nMODE(s): %s\n" (String.concat " " (List.rev_map show_mode t.mode)); *)
  match t.mode with
  | Template :: _ -> scan_template_token ~start_pos t
  | TemplateAttributes :: _ -> scan_template_attributes_token ~start_pos t
  | ComponentAttributes :: _ -> scan_component_attributes_token ~start_pos t
  | [] | Normal :: _ -> scan_normal_token ~start_pos t
;;

let scan t =
  if not (inTemplateMode t) then skip_whitespace t;
  let start_pos = make_position t in
  let token = scan_token ~start_pos t in
  (* print_endline (Token.to_string token); *)
  let end_pos = make_position t in
  Token.make ~start_pos ~end_pos token
;;
