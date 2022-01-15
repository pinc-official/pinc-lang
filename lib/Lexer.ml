type mode = Normal | TemplateAttributes | Template | TemplateBlock
[@@deriving show { with_path = false }]

type char' = [ | `Chr of char | `EOF ]

type t = {
  filename: string;
  src : string;
  mutable prev : [ char' | `BOF];
  mutable current : char';
  mutable next : char';

  mutable offset : int;

  mutable line_offset : int;
  mutable line : int;
  mutable column : int;

  mutable mode : mode list;
}

let make_position a = 
  Position.make
    ~filename: a.filename
    ~line: a.line
    ~column: (a.column + 1)


let inNormalMode t = match t.mode with
  | Normal::_ -> true
  | _ -> false

let setNormalMode t = t.mode <- Normal :: t.mode

let popNormalMode t = begin
  match t.mode with
  | Normal::tl -> t.mode <- tl
  | _ -> ()
end

let inTemplateMode t = match t.mode with
  | Template::_ -> true
  | _ -> false

let setTemplateMode t = t.mode <- Template :: t.mode

let popTemplateMode t = begin
  match t.mode with
  | Template::tl -> t.mode <- tl
  | _ -> ()
end

let inTemplateBlockMode t = match t.mode with
  | TemplateBlock::_ -> true
  | _ -> false

let setTemplateBlockMode t = t.mode <- TemplateBlock :: t.mode

let popTemplateBlockMode t = begin
  match t.mode with
  | TemplateBlock::tl -> t.mode <- tl
  | _ -> ()
end

let inTemplateAttributesMode t = match t.mode with
  | TemplateAttributes::_ -> true
  | _ -> false

let setTemplateAttributesMode t = t.mode <- TemplateAttributes :: t.mode

let popTemplateAttributesMode t = begin
  match t.mode with
  | TemplateAttributes::tl -> t.mode <- tl
  | _ -> ()
end


let next t = begin
  let next_offset = t.offset + 1 in
  let () = match t.current with
    | `Chr '\n' ->
      t.line_offset <- next_offset;
      t.line        <- succ t.line;
    | _ -> ()
  in
  t.column  <- next_offset - t.line_offset;
  t.offset  <- next_offset;
  t.prev    <- (match t.current with  | `EOF -> `EOF | `Chr c -> `Chr c);
  t.current <- if next_offset < String.length t.src
    then `Chr (String.unsafe_get t.src next_offset)
    else `EOF;
  t.next <- if next_offset + 1 < String.length t.src
    then `Chr (String.unsafe_get t.src (next_offset + 1))
    else `EOF;
end

let next_n ~n t = begin
  for _ = 1 to n do
    next t;
  done;
end

let peek t = t.next

let make ~filename src = {
  filename;
  src = src;
  prev = `BOF;
  current = if src = "" then `EOF else `Chr (String.unsafe_get src 0);
  next = if String.length src < 2 then `EOF else `Chr (String.unsafe_get src 1);

  offset = 0;
  line_offset = 0;
  column = 0;
  line = 1;

  mode = [Normal];
}

let is_whitespace = function
  | `Chr ' ' | `Chr '\t' | `Chr '\n' | `Chr '\r' -> true
  | _ -> false

let rec skip_whitespace t = 
  if is_whitespace t.current then begin
    next t;
    skip_whitespace t;
  end

let scan_ident t = begin
  (* NOTE: List all vaid chars for identifiers here: *)
  let rec loop acc t =
    match t.current with
    | `Chr ('A'..'Z' as c)
    | `Chr ('a'..'z' as c)
    | `Chr ('0'..'9' as c)
    | `Chr ('_' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  let found = loop "" t in
  Token.lookup_keyword found
end

let scan_string t = begin
  let start_pos = make_position t in
  let rec loop acc t =
    match t.current with
    | `EOF -> Diagnostics.report ~start_pos ~end_pos:(make_position t) Diagnostics.NonTerminatedString
    | `Chr '\\' -> (
      match peek t with 
      | `Chr ' '  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\\') t
      | `Chr '"'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '"') t
      | `Chr '\'' -> next_n ~n:2 t; loop (acc ^ String.make 1 '\'') t
      | `Chr 'b'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\b') t
      | `Chr 'f'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\012') t
      | `Chr 'n'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\n') t
      | `Chr 'r'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\r') t
      | `Chr 't'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\t') t
      | `Chr '\\' -> next_n ~n:2 t; loop (acc ^ String.make 1 '\\') t
      | `Chr _    -> Diagnostics.report ~start_pos:(make_position t) ~end_pos:(make_position t) (Diagnostics.Message "Unknown escape sequence in string.")
      | `EOF      -> Diagnostics.report ~start_pos ~end_pos:(make_position t) Diagnostics.NonTerminatedString
    )
    | `Chr '"' -> next t; acc
    | `Chr c ->
      next t;
      loop (acc ^ String.make 1 c) t
  in
  (* NEXT once to skip the beginning double quote *)
  next t;
  let found = loop "" t in
  Token.STRING found
end

let scan_symbol_or_template t = begin
  let start_pos = make_position t in
  let rec loop acc t =
    match t.current with
    | `Chr ('A'..'Z' as c)
    | `Chr ('a'..'z' as c)
    | `Chr ('0'..'9' as c)
    | `Chr ('_' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  (* NEXT once to skip the beginning @ symbol *)
  next t;
  let found = loop "" t in
  match found with
  | "Template" -> Token.TEMPLATE
  | s          -> begin
    match s.[0] with
    | 'A'..'Z' -> Token.SYMBOL found
    | _ -> 
      Diagnostics.report ~start_pos ~end_pos:(make_position t)
      (Diagnostics.Message (Printf.sprintf "Invalid Symbol! Symbols have to start with an uppercase character. Did you mean to write @%s?" (String.capitalize_ascii found)))
  end
end

let get_html_tag_ident t = begin
  let start_pos = make_position t in
  let rec loop acc t =
    match t.current with
    | `Chr ('a'..'z' as c)
    | `Chr ('-' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  let ident = loop "" t in
  skip_whitespace t;
  match ident.[0] with
  | 'a'..'z' -> ident
  | _ -> 
    Diagnostics.report ~start_pos ~end_pos:(make_position t)
      (Diagnostics.Message (Printf.sprintf "Invalid HTML tag! HTML tags have to start with a lowercase letter. Instead saw: %s" ident))
end

let get_uppercase_ident t = begin
  (* NOTE: List all vaid chars for identifiers here: *)
  let rec loop acc t =
    match t.current with
    | `Chr ('A'..'Z' as c)
    | `Chr ('a'..'z' as c)
    | `Chr ('0'..'9' as c)
    | `Chr ('_' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  loop "" t
end

let scan_open_tag t = begin
  let start_pos = make_position t in
  (* NEXT once to skip the beginning < char *)
  next t;
  match t.current with
  | `Chr 'a'..'z' -> Token.HTML_OPEN_TAG (get_html_tag_ident t)
  | `Chr 'A'..'Z' -> Token.COMPONENT_OPEN_TAG (get_uppercase_ident t)
  | `Chr c        ->
    Diagnostics.report ~start_pos ~end_pos:(make_position t)
      (Diagnostics.Message (Printf.sprintf "Invalid Template tag! Template tags have to start with an uppercase or lowercase letter. Instead saw: %c" c))
  | `EOF -> Diagnostics.report ~start_pos ~end_pos:(make_position t) Diagnostics.NonTerminatedTemplate
end

let scan_close_tag t = begin
  let start_pos = make_position t in
  (* NEXT twice to skip the beginning </ chars *)
  next_n ~n:2 t;
  let close_tag = match t.current with
  | `Chr 'a'..'z' -> Token.HTML_CLOSE_TAG (get_html_tag_ident t)
  | `Chr 'A'..'Z' -> Token.COMPONENT_CLOSE_TAG (get_uppercase_ident t)
  | `Chr c        ->
    Diagnostics.report ~start_pos ~end_pos:(make_position t)
      (Diagnostics.Message (Printf.sprintf "Invalid Template tag! Template tags have to start with an uppercase or lowercase letter. Instead saw: %c" c))
  | `EOF -> Diagnostics.report ~start_pos ~end_pos:(make_position t) Diagnostics.NonTerminatedTemplate
  in
  match t.current with
  | `Chr '>' -> next t; close_tag
  | _        -> Diagnostics.report ~start_pos ~end_pos:(make_position t) (ExpectedToken Token.GREATER)
end

let scan_template_text t = begin
  let start_pos = make_position t in
  let rec loop acc t =
    match t.current with
    | `EOF -> Diagnostics.report ~start_pos ~end_pos:(make_position t) Diagnostics.NonTerminatedTemplate
    | `Chr '<' -> (
      match peek t with
      | `Chr '/'      -> acc
      | `Chr 'a'..'z' -> acc
      | _             -> next t; loop (acc ^ "<") t
    )
    | `Chr c ->
      next t;
      loop (acc ^ String.make 1 c) t
  in
  let found = loop "" t in
  Token.STRING found
end

let scan_html_ident t = begin
  (* NOTE: List all vaid chars for html identifiers here: *)
  let rec loop acc t =
    match t.current with
    | `Chr ('A'..'Z' as c)
    | `Chr ('a'..'z' as c)
    | `Chr ('0'..'9' as c)
    | `Chr ('-' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  let found = loop "" t in
  Token.lookup_keyword found
end

let scan_number t = begin
  let result = Buffer.create 5 in

  let rec scan_digits t =
    match t.current with
    | `Chr ('0'..'9' as c) -> Buffer.add_char result c; next t; scan_digits t
    | `Chr '_'             -> next t; scan_digits t
    | _                   -> ()
  in

  scan_digits t;

  let is_float = match t.current with
  | `Chr '.' -> Buffer.add_char result '.'; next t; scan_digits t; true
  | _        -> false
  in

  (* exponent part *)
  let is_float = match t.current with
  | `Chr 'e' | `Chr 'E' ->
    Buffer.add_char result 'e'; next t;
    let () = match t.current with
    | `Chr '+' -> Buffer.add_char result '+'; next t
    | `Chr '-' -> Buffer.add_char result '-'; next t
    | _ -> ()
    in
    scan_digits t;
    true
  | _ -> is_float
  in

  let result = Buffer.contents result in

  if is_float then
    Token.FLOAT (float_of_string result)
  else
    Token.INT (int_of_string result)
end

let skip_comment t = begin
  let rec skip t =
    match t.current with
    | `Chr '\n' | `Chr '\r' -> ()
    | `EOF -> ()
    | _ ->
      next t;
      skip t
  in
  skip t
end

let scan t = begin
  skip_whitespace t;
  let start_pos = make_position t in

  (* Printf.printf "\nMODE(s): %s\n" (String.concat " " (List.map show_mode t.mode)); *)

  let token = if inTemplateMode t then begin match t.current with
    | `Chr '{' -> setNormalMode t; next t; Token.LEFT_BRACE
    | `Chr '}' -> popNormalMode t; next t; Token.RIGHT_BRACE
    | `Chr '<' -> (
      match peek t with
      | `Chr 'a'..'z'
      | `Chr 'A'..'Z' -> setTemplateMode t; setTemplateAttributesMode t; scan_open_tag t;
      | `Chr '/'      -> popTemplateMode t; scan_close_tag t;
      | _             -> scan_template_text t
    )
    | `EOF     -> Diagnostics.report ~start_pos ~end_pos:(make_position t) Diagnostics.NonTerminatedTemplate
    | _        -> scan_template_text t
  end else begin match t.current with
    | `Chr 'A'..'Z' | `Chr 'a'..'z' when inTemplateAttributesMode t -> scan_html_ident t
    | `Chr 'A'..'Z' | `Chr 'a'..'z' | `Chr '_' -> scan_ident t
    | `Chr '0'..'9' -> scan_number t
    
    | `Chr '"' -> scan_string t
    | `Chr '(' -> next t; Token.LEFT_PAREN
    | `Chr ')' -> next t; Token.RIGHT_PAREN
    | `Chr '[' -> next t; Token.LEFT_BRACK
    | `Chr ']' -> next t; Token.RIGHT_BRACK
    | `Chr '{' -> next t; Token.LEFT_BRACE
    | `Chr '}' -> next t; Token.RIGHT_BRACE
    | `Chr ':' -> next t; Token.COLON
    | `Chr ',' -> next t; Token.COMMA
    | `Chr ';' -> next t; Token.SEMICOLON
    | `Chr '-' -> next t; Token.MINUS
    | `Chr '+' -> next t; Token.PLUS
    | `Chr '%' -> next t; Token.PERCENT
    | `Chr '?' -> next t; Token.QUESTIONMARK
    
    | `Chr '/' -> (
      match peek t with
      | `Chr '/' -> skip_comment t; Token.COMMENT
      | _        -> next t; Token.SLASH
    )
    | `Chr '.' -> (
      match peek t with
      | `Chr '0'..'9' -> scan_number t
      | _            -> next t; Token.DOT
    )
    | `Chr '@' -> (
      match peek t with
      | `Chr 'A'..'Z' | `Chr 'a'..'z'    -> scan_symbol_or_template t
      | _ -> Diagnostics.report ~start_pos ~end_pos:(make_position t) (Diagnostics.UnknownCharacter '@')
    )
    | `Chr '&' -> (
      match peek t with
      | `Chr '&' -> next_n ~n:2 t; Token.LOGICAL_AND
      | _       -> Diagnostics.report ~start_pos ~end_pos:(make_position t) (Diagnostics.UnknownCharacter '&')
    )
    | `Chr '|' -> (
      match peek t with
      | `Chr '|' -> next_n ~n:2 t; Token.LOGICAL_OR
      | _       -> next t; Token.PIPE
    )
    | `Chr '!' -> (
      match peek t with
      | `Chr '=' -> next_n ~n:2 t; Token.NOT_EQUAL
      | _       -> next t; Token.NOT
    )
    | `Chr '=' -> (
      match peek t with
      | `Chr '=' -> next_n ~n:2 t; Token.EQUAL_EQUAL
      | _       -> next t; Token.EQUAL
    )
    | `Chr '>' -> (
      popTemplateAttributesMode t;
      if t.prev = `Chr '/' then popTemplateMode t;
      match peek t with
      | `Chr '=' -> next_n ~n:2 t; Token.GREATER_EQUAL
      | _        -> next t; Token.GREATER
    )
    | `Chr '*' -> (
      match peek t with
      | `Chr '*' -> next_n ~n:2 t; Token.STAR_STAR
      | _       -> next t; Token.STAR
    )
    | `Chr '<' -> (
      match peek t with
      | `Chr '='      -> next_n ~n:2 t; Token.LESS_EQUAL
      | `Chr '/'      -> popTemplateMode t; scan_close_tag t;
      | `Chr 'a'..'z'
      | `Chr 'A'..'Z' -> setTemplateMode t; setTemplateAttributesMode t; scan_open_tag t;
      | c when ((is_whitespace t.prev) && (is_whitespace c)) -> next t; Token.LESS
      | _             -> Diagnostics.report ~start_pos ~end_pos:(make_position t) (Diagnostics.UnknownCharacter '<')
    )

    | `EOF -> Token.END_OF_INPUT
    | `Chr c -> Diagnostics.report ~start_pos ~end_pos:(make_position t) (Diagnostics.UnknownCharacter c) (* NOTE: Maybe we can ignore this and continue scanning... *)
  end in
  (* print_endline (Token.to_string token); *)
  let end_pos = make_position t in
  Token.make ~start_pos ~end_pos token
end