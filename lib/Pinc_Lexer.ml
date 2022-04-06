module Diagnostics = Pinc_Diagnostics
module Position = Pinc_Position
module Token = Pinc_Token

type mode =
  | Normal
  | String
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

(* let show_mode = function
  | Normal -> "NORMAL"
  | String -> "String"
  | TemplateAttributes -> "TemplateAttributes"
  | ComponentAttributes -> "ComponentAttributes"
  | Template -> "Template"
;; *)

let current_mode t =
  match t.mode with
  | [] -> Normal
  | Normal :: _ -> Normal
  | String :: _ -> String
  | Template :: _ -> Template
  | TemplateAttributes :: _ -> TemplateAttributes
  | ComponentAttributes :: _ -> ComponentAttributes
;;

(* let previous_mode t =
  match t.mode with
  | [] | [ _ ] -> Normal
  | _ :: Normal :: _ -> Normal
  | _ :: String :: _ -> String
  | _ :: Template :: _ -> Template
  | _ :: TemplateAttributes :: _ -> TemplateAttributes
  | _ :: ComponentAttributes :: _ -> ComponentAttributes
;; *)

let setMode mode t = t.mode <- mode :: t.mode

let popMode mode t =
  match t.mode with
  | current_mode :: tl when current_mode = mode -> t.mode <- tl
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

let scan_string ~start_pos t =
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
    | `Chr '{' when peek t = `Chr '|' -> ()
    | `Chr '"' -> ()
    | `Chr c ->
      next t;
      Buffer.add_char buf c;
      loop t
  in
  let () = loop t in
  Token.STRING (Buffer.contents buf)
;;

let scan_tag t =
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
  let () = loop t in
  let found = Buffer.contents buf in
  match found.[0] with
  | 'A' .. 'Z' -> found
  | _ ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      (Diagnostics.Message
         ("Invalid Tag! Tags have to start with an uppercase character. Did you mean to \
           write #"
         ^ String.capitalize_ascii found
         ^ "?"))
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
         ("Invalid HTML tag! HTML tags have to start with a lowercase letter. Instead \
           saw: "
         ^ ident))
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

let scan_component_open_tag t =
  let start_pos = make_position t in
  (* NEXT once to skip the beginning < char *)
  next t;
  match t.current with
  | `Chr 'A' .. 'Z' -> Token.COMPONENT_OPEN_TAG (get_uppercase_ident t)
  | `Chr c ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      (Diagnostics.Message
         ("Invalid Component tag! Component tags have to start with an uppercase letter. \
           Instead saw: "
         ^ String.make 1 c))
  | `EOF ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      Diagnostics.NonTerminatedTemplate
;;

let scan_open_tag t =
  let start_pos = make_position t in
  (* NEXT once to skip the beginning < char *)
  next t;
  match t.current with
  | `Chr 'a' .. 'z' -> Token.HTML_OPEN_TAG (get_html_tag_ident t)
  | `Chr c ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      (Diagnostics.Message
         ("Invalid Template tag! Template tags have to start with a lowercase letter. \
           Instead saw: "
         ^ String.make 1 c))
  | `EOF ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      Diagnostics.NonTerminatedTemplate
;;

let scan_component_close_tag t =
  let start_pos = make_position t in
  (* NEXT twice to skip the beginning </ chars *)
  next_n ~n:2 t;
  let close_tag =
    match t.current with
    | `Chr 'A' .. 'Z' -> get_uppercase_ident t
    | `Chr c ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.Message
           ("Invalid Component tag! Component tags have to start with an uppercase \
             letter. Instead saw: "
           ^ String.make 1 c))
    | `EOF ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        Diagnostics.NonTerminatedTemplate
  in
  match t.current with
  | `Chr '>' ->
    next t;
    Token.COMPONENT_CLOSE_TAG close_tag
  | _ ->
    Diagnostics.report ~start_pos ~end_pos:(make_position t) (ExpectedToken Token.GREATER)
;;

let scan_close_tag t =
  let start_pos = make_position t in
  (* NEXT twice to skip the beginning </ chars *)
  next_n ~n:2 t;
  let close_tag =
    match t.current with
    | `Chr 'a' .. 'z' -> Token.HTML_CLOSE_TAG (get_html_tag_ident t)
    | `Chr c ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.Message
           ("Invalid Template tag! Template tags have to start with a lowercase letter. \
             Instead saw: "
           ^ String.make 1 c))
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
    setMode Normal t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popMode Normal t;
    next t;
    Token.RIGHT_BRACE
  | `Chr '<' ->
    (match peek t with
    | `Chr '>' ->
      next_n ~n:2 t;
      setMode Template t;
      Token.HTML_OPEN_FRAGMENT
    | `Chr 'a' .. 'z' ->
      setMode Template t;
      setMode TemplateAttributes t;
      scan_open_tag t
    | `Chr 'A' .. 'Z' ->
      setMode Template t;
      setMode ComponentAttributes t;
      scan_component_open_tag t
    | `Chr '/' ->
      popMode Template t;
      (match peek ~n:2 t with
      | `Chr 'a' .. 'z' -> scan_close_tag t
      | `Chr 'A' .. 'Z' -> scan_component_close_tag t
      | `Chr '>' ->
        next_n ~n:3 t;
        Token.HTML_CLOSE_FRAGMENT
      | `Chr c ->
        Diagnostics.report
          ~start_pos
          ~end_pos:(make_position t)
          (Diagnostics.Message
             ("Invalid Template tag! Template tags have to start with an uppercase or \
               lowercase letter. Instead saw: "
             ^ String.make 1 c))
      | `EOF ->
        Diagnostics.report
          ~start_pos
          ~end_pos:(make_position t)
          Diagnostics.NonTerminatedTemplate)
    | _ -> scan_template_text t)
  | `Chr '/' ->
    (match peek t with
    | `Chr '>' ->
      popMode Template t;
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

and scan_string_token ~start_pos t =
  match t.current with
  | `Chr '"' ->
    popMode String t;
    next t;
    Token.DOUBLE_QUOTE
  | `Chr '{' when peek t = `Chr '|' ->
    setMode Normal t;
    next_n ~n:2 t;
    Token.LEFT_PIPE_BRACE
  | `Chr '|' when peek t = `Chr '}' ->
    popMode Normal t;
    next_n ~n:2 t;
    Token.RIGHT_PIPE_BRACE
  | _ -> scan_string ~start_pos t

and scan_component_attributes_token ~start_pos t =
  match t.current with
  | `Chr '_' | `Chr 'a' .. 'z' -> scan_ident t
  | `Chr '"' ->
    next t;
    setMode String t;
    Token.DOUBLE_QUOTE
  | `Chr '=' ->
    next t;
    Token.EQUAL
  | `Chr '{' ->
    setMode Normal t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popMode Normal t;
    next t;
    Token.RIGHT_BRACE
  | `Chr '/' ->
    (match peek t with
    | `Chr '>' ->
      popMode ComponentAttributes t;
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
    popMode ComponentAttributes t;
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
  | `Chr '"' ->
    next t;
    setMode String t;
    Token.DOUBLE_QUOTE
  | `Chr '=' ->
    next t;
    Token.EQUAL
  | `Chr '{' ->
    setMode Normal t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popMode Normal t;
    next t;
    Token.RIGHT_BRACE
  | `Chr '/' ->
    (match peek t with
    | `Chr '>' ->
      popMode TemplateAttributes t;
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
    popMode TemplateAttributes t;
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
  | `Chr '"' ->
    next t;
    setMode String t;
    Token.DOUBLE_QUOTE
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
    setMode Normal t;
    next t;
    Token.LEFT_BRACE
  | `Chr '}' ->
    popMode Normal t;
    next t;
    Token.RIGHT_BRACE
  | `Chr ':' ->
    (match peek t with
    | `Chr ':' ->
      next_n ~n:2 t;
      Token.DOUBLE_COLON
    | `Chr '=' ->
      next_n ~n:2 t;
      Token.COLON_EQUAL
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
    | `Chr 'A' .. 'Z' ->
      next t;
      Token.TAG (scan_tag t)
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
    | `Chr '}' ->
      popMode Normal t;
      next_n ~n:2 t;
      Token.RIGHT_PIPE_BRACE
    | `Chr '>' ->
      next_n ~n:2 t;
      Token.PIPE
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
    | `Chr '>' ->
      next_n ~n:2 t;
      Token.HTML_OPEN_FRAGMENT
    | `Chr '-' ->
      next_n ~n:2 t;
      Token.ARROW_LEFT
    | `Chr '=' ->
      next_n ~n:2 t;
      Token.LESS_EQUAL
    | `Chr 'a' .. 'z' ->
      setMode Template t;
      setMode TemplateAttributes t;
      scan_open_tag t
    | `Chr 'A' .. 'Z' ->
      setMode Template t;
      setMode ComponentAttributes t;
      scan_component_open_tag t
    | c when is_whitespace t.prev && is_whitespace c ->
      next t;
      Token.LESS
    | `Chr '/' ->
      popMode Template t;
      (match peek ~n:2 t with
      | `Chr '>' ->
        next_n ~n:3 t;
        Token.HTML_CLOSE_FRAGMENT
      | `Chr 'a' .. 'z' -> scan_close_tag t
      | `Chr 'A' .. 'Z' -> scan_component_close_tag t
      | `Chr c ->
        Diagnostics.report
          ~start_pos
          ~end_pos:(make_position t)
          (Diagnostics.Message
             ("Invalid Template tag! Template tags have to start with an uppercase or \
               lowercase letter. Instead saw: "
             ^ String.make 1 c))
      | `EOF ->
        Diagnostics.report
          ~start_pos
          ~end_pos:(make_position t)
          Diagnostics.NonTerminatedTemplate)
    | _ ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(make_position t)
        (Diagnostics.UnknownCharacter '<'))
  | `EOF -> Token.END_OF_INPUT
  | `Chr c ->
    Diagnostics.report
      ~start_pos
      ~end_pos:(make_position t)
      (Diagnostics.UnknownCharacter c)
(* NOTE: Maybe we can ignore this and continue scanning... *)
(* next t;
    scan_token ~start_pos t *)

and scan_token ~start_pos t =
  (* Printf.printf "\nMODE(s): %s\n" (String.concat " " (List.rev_map show_mode t.mode)); *)
  match current_mode t with
  | Template -> scan_template_token ~start_pos t
  | TemplateAttributes -> scan_template_attributes_token ~start_pos t
  | ComponentAttributes -> scan_component_attributes_token ~start_pos t
  | String -> scan_string_token ~start_pos t
  | Normal -> scan_normal_token ~start_pos t
;;

let scan t =
  if not (current_mode t = Template || current_mode t = String) then skip_whitespace t;
  let start_pos = make_position t in
  let token = scan_token ~start_pos t in
  (* print_endline (Token.to_string token); *)
  let end_pos = make_position t in
  Token.make ~start_pos ~end_pos token
;;