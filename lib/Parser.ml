type t = {
  mutable lexer: Lexer.t;
  mutable token: Token.t;
  mutable prev_token: Token.t option;
}

let next ?prev_token t = begin
  let prev_token = prev_token |> Option.value ~default:t.token in
  let token = Lexer.scan t.lexer in

  t.token      <- token;
  t.prev_token <- Some prev_token;
end

let optional token t =
  let test = t.token.typ = token in
  if test then next t;
  test

let expect token t = begin
  let test = t.token.typ = token in
  if test then
    next t
  else (
    print_endline (Token.to_string t.token.typ);
    assert false; (* TODO: *)
  )
  
end

let make ~filename src = begin
  let lexer = Lexer.make ~filename src in
  let initial_pos = Position.make ~filename ~line:0 ~column: 0 in
  let initial_token = Token.make ~start_pos: initial_pos ~end_pos: initial_pos Token.END_OF_INPUT in
  let t = {
    lexer      = lexer;
    token      = initial_token;
    prev_token = None;
  } in
  next t;
  t
end

let parse_region ?(list=false) ~f t = begin
  let rec loop acc =
    match f t with
    | Some r -> loop (r::acc)
    | None when t.token.typ = Token.END_OF_INPUT || list -> List.rev acc
    | None -> assert false (* TODO: *)
    in
  loop []
end

let parse_attr_value t = begin
  match t.token.typ with
  | Token.STRING s -> next t; Ast_Value.String s
  | _ -> assert false
end

let parse_attr t = begin
  match t.token.typ with
  | Token.IDENT_LOWER ident ->
    next t;
    expect Token.COLON t;
    let value = parse_attr_value t in
    optional Token.COMMA t |> ignore;
    Some (Ast_Attr.make ident value)
  | _ -> None
end

let parse_symbol_attrs t = begin
  parse_region t ~list:true ~f:parse_attr
end

let parse_symbol t = begin
  match t.token.typ with
  | Token.SYMBOL ident ->
    next t;
    begin match t.token.typ with
    | Token.LEFT_PAREN -> 
      next t;
      let attrs = parse_symbol_attrs t in
      expect Token.RIGHT_PAREN t;
      Some (Ast_Symbol.make ident attrs)
    | _ -> None
    end
  | _ -> None
end

let parse_symbols t = begin
  parse_region t ~list:true ~f:parse_symbol
end

let scan t = begin
  let start_pos = t.token.start_pos in
  let symbols = parse_symbols t in
  print_endline (Token.to_string t.token.typ);
  let declarations = match t.token.typ with
  | Token.KEYWORD_SITE ->
    next t;
    begin match t.token.typ with
    | Token.IDENT_UPPER name ->
      Ast_Declaration.make_component ~symbols ~properties:[] ~template:"" name
    | _ -> assert false (* TODO: *)
    end
  | _ -> assert false (* TODO: *)
  in
  let declarations = List.init 1 (fun _ -> declarations) in
  Ast.{loc = start_pos; declarations}
end