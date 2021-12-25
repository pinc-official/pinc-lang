exception SyntaxError of string

type mode = Normal | Template

type char' = Chr of Char.t | EOF

type t = {
  filename: string;
  src : string;
  mutable current : char';

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
    ~column: a.column


let inNormalMode t = match t.mode with
  | Normal::_ -> true
  | _ -> false

let inTemplateMode t = match t.mode with
  | Template::_ -> true
  | _ -> false

let popMode t mode = begin
  match t.mode with
  | hd::tl when hd = mode ->
    t.mode <- tl
  | _ -> ()
end

let next t =
  let next_offset = t.offset + 1 in
  let () = match t.current with
    | Chr '\n' ->
      t.line_offset <- next_offset;
      t.line        <- succ t.line;
    | _ -> ()
  in
  if next_offset < String.length t.src then (
    t.offset  <- next_offset;
    t.current <- Chr (String.unsafe_get t.src t.offset);
  ) else (
    t.offset  <- String.length t.src;
    t.current <- EOF
  )

let next_n ~n t = begin
  for _ = 1 to n do
    next t;
  done;
end

let peek ?(n=1) t =
  if t.offset + n < String.length t.src then
    Chr (String.unsafe_get t.src (t.offset + 1))
  else
    EOF

let make ~filename src = {
  filename;
  src = src;
  current = if src = "" then EOF else Chr (String.unsafe_get src 0);

  offset = 0;
  line_offset = 0;
  column = 0;
  line = 1;

  mode = [];
}

let is_whitespace = function
  | Chr ' ' | Chr '\t' | Chr '\n' | Chr '\r' -> true
  | _ -> false

let rec skip_whitespace t = 
  if is_whitespace t.current then begin
    next t;
    skip_whitespace t;
  end

let digitValue ch =
  match ch with
  | Chr ('0'..'9' as c) -> (Char.code c) - 48
  | Chr ('a'..'f' as c) ->
    (Char.code c) - (Char.code 'a') + 10
  | Chr ('A'..'F' as c) ->
    (Char.code c) + 32 - (Char.code 'a') + 10
  | _ -> 16

let scan_ident t = begin
  (* NOTE: List all vaid chars for identifiers here: *)
  let rec loop acc t =
    match t.current with
    | Chr ('A'..'Z' as c)
    | Chr ('a'..'z' as c)
    | Chr ('0'..'9' as c)
    | Chr ('_' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  let found = loop "" t in
  Token.lookup_keyword found
end

let scan_string t = begin
  let rec loop acc t =
    match t.current with
    | EOF -> assert false (* TODO: Report unterminated string *)
    | Chr '\\' -> (
      match peek t with 
      | Chr ' '  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\\') t
      | Chr '"'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '"') t
      | Chr '\'' -> next_n ~n:2 t; loop (acc ^ String.make 1 '\'') t
      | Chr 'b'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\b') t
      | Chr 'f'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\012') t
      | Chr 'n'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\n') t
      | Chr 'r'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\r') t
      | Chr 't'  -> next_n ~n:2 t; loop (acc ^ String.make 1 '\t') t
      | Chr '\\' -> next_n ~n:2 t; loop (acc ^ String.make 1 '\\') t
      | Chr _    -> assert false (* TODO: Report unknown escape *)
      | EOF      -> assert false (* TODO: Report unterminated string *)
    )
    | Chr '"' -> next t; acc
    | Chr c ->
      next t;
      loop (acc ^ String.make 1 c) t
  in
  (* NEXT once to skip the beginning double quote *)
  next t;
  let found = loop "" t in
  Token.STRING found
end

let scan_symbol t = begin
  let rec loop acc t =
    match t.current with
    | Chr ('A'..'Z' as c)
    | Chr ('a'..'z' as c)
    | Chr ('0'..'9' as c)
    | Chr ('_' as c) ->
      next t;
      loop (acc ^ String.make 1 c) t
    | _ -> acc
  in
  (* NEXT once to skip the beginning @ symbol *)
  next t;
  let found = loop "" t in
  match found.[0] with
  | 'A'..'Z' -> Token.SYMBOL found
  | _ -> assert false (* TODO: Report invalid symbol *)
end

let scan_number t = begin
  let result = Buffer.create 5 in

  let rec scan_digits t =
    match t.current with
    | Chr ('0'..'9' as c) -> Buffer.add_char result c; next t; scan_digits t
    | Chr '_'             -> next t; scan_digits t
    | _                   -> ()
  in

  scan_digits t;

  let is_float = match t.current with
  | Chr '.' -> Buffer.add_char result '.'; next t; scan_digits t; true
  | _       -> false
  in

  (* exponent part *)
  let is_float = match t.current with
  | Chr 'e' | Chr 'E' ->
    Buffer.add_char result 'e'; next t;
    let () = match t.current with
    | Chr '+' -> Buffer.add_char result '+'; next t
    | Chr '-' -> Buffer.add_char result '-'; next t
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
    | Chr '\n' | Chr '\r' -> ()
    | EOF -> ()
    | _ ->
      next t;
      skip t
  in
  skip t
end

let rec scan t = begin
  skip_whitespace t;
  let start_pos = make_position t in

  let token = match t.current with
  | Chr 'A'..'Z' | Chr 'a'..'z' | Chr '_' -> scan_ident t
  | Chr '0'..'9' -> scan_number t
  
  | Chr '"' -> scan_string t
  | Chr '(' -> next t; Token.LEFT_PAREN
  | Chr ')' -> next t; Token.RIGHT_PAREN
  | Chr '[' -> next t; Token.LEFT_BRACK
  | Chr ']' -> next t; Token.RIGHT_BRACK
  | Chr '{' -> next t; Token.LEFT_BRACE
  | Chr '}' -> next t; Token.RIGHT_BRACE
  | Chr ':' -> next t; Token.COLON
  | Chr ',' -> next t; Token.COMMA
  | Chr ';' -> next t; Token.SEMICOLON
  | Chr '-' -> next t; Token.MINUS
  | Chr '+' -> next t; Token.PLUS
  | Chr '/' -> next t; Token.SLASH
  | Chr '%' -> next t; Token.PERCENT
  
  | Chr '.' -> (
    match peek t with
    | Chr '0'..'9' -> scan_number t
    | _            -> next t; Token.DOT
  )
  | Chr '@' -> (
    match peek t with
    | Chr 'A'..'Z' -> scan_symbol t
    | Chr ' '      -> assert false (* TODO: REPORT UNKNOWN TOKEN @ *)
    | _            -> assert false (* TODO: Report invalid symbol syntax *)
  )
  | Chr '&' -> (
    match peek t with
    | Chr '&' -> next_n ~n:2 t; Token.LOGICAL_AND
    | _       -> assert false (* TODO: REPORT UNKNOWN TOKEN & *)
  )
  | Chr '|' -> (
    match peek t with
    | Chr '|' -> next_n ~n:2 t; Token.LOGICAL_OR
    | _       -> next t; Token.PIPE
  )
  | Chr '!' -> (
    match peek t with
    | Chr '=' -> next_n ~n:2 t; Token.NOT_EQUAL
    | _       -> next t; Token.NOT
  )
  | Chr '=' -> (
    match peek t with
    | Chr '=' -> next_n ~n:2 t; Token.EQUAL_EQUAL
    | _       -> next t; Token.EQUAL
  )
  | Chr '>' -> (
    match peek t with
    | Chr '=' -> next_n ~n:2 t; Token.GREATER_EQUAL
    | _       -> next t; Token.GREATER
  )
  | Chr '<' -> (
    match peek t with
    | Chr '=' -> next_n ~n:2 t; Token.LESS_EQUAL
    | _       -> next t; Token.LESS
  )
  | Chr '*' -> (
    match peek t with
    | Chr '*' -> next_n ~n:2 t; Token.STAR_STAR
    | _       -> next t; Token.STAR
  )

  | EOF -> Token.END_OF_INPUT
  | Chr _c -> 
      (* if we arrive here, we're dealing with an unknown character,
       * report the error and continue scanning... *)
      next t;
      (* let end_pos = make_position t in *)
      (* scanner.err ~startPos ~endPos (Diagnostics.unknownUchar ch); *)
      let Token.{typ; _} = scan t in
      typ
  in
  let end_pos = make_position t in
  Token.make ~start_pos ~end_pos token
end