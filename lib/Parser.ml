exception Parser_Error of string

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
    raise (Parser_Error (Printf.sprintf "Expected: %s, got %s" (Token.to_string token) (Token.to_string t.token.typ)))
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

module Helpers = struct
  let separated_list ~sep ~fn t = begin
    let rec loop acc = begin
      let has_sep = optional sep t in
      match fn t with
      | Some r -> 
        if has_sep || acc = []
          then loop (r::acc)
          else raise (Parser_Error (Printf.sprintf "Expected list to be separated by %s" (Token.to_string sep)))
      | None   -> List.rev acc
    end in
    loop []
  end


  let non_empty_separated_list ~sep ~fn t = begin
    let res = separated_list ~sep ~fn t in
    if res = [] then raise (Parser_Error (Printf.sprintf "Expected list to not be empty"));
    res
  end

  let list ~fn t = begin
    let rec loop acc = begin
      match fn t with
      | Some r -> loop (r::acc)
      | None   -> List.rev acc
    end in
    loop []
  end

  let non_empty_list ~fn t = begin
    let res = list ~fn t in
    if res = [] then raise (Parser_Error (Printf.sprintf "Expected list to not be empty"));
    res
  end
end

module Rules = struct
  let rec value t = begin
    match t.token.typ with
    | Token.STRING s      -> next t; Some (Ast_Value.String s)
    | Token.INT i         -> next t; Some (Ast_Value.Int i)
    | Token.FLOAT f       -> next t; Some (Ast_Value.Float f)
    | Token.KEYWORD_TRUE  -> next t; Some (Ast_Value.Bool true)
    | Token.KEYWORD_FALSE -> next t; Some (Ast_Value.Bool false)
    | Token.LEFT_BRACK    -> begin
        next t;
        let values = Helpers.separated_list ~sep:Token.COMMA ~fn:value t in
        expect Token.RIGHT_BRACK t;
        Some (Ast_Value.Array values)
      end
    | _ -> None
  end
  
  let attr t = begin
    match t.token.typ with
    | Token.IDENT_LOWER ident ->
      next t;
      expect Token.COLON t;
      begin match value t with
      | Some v -> Some (Ast_Attr.make ident v)
      | None -> None
      end
    | _ -> None
  end
  
  let symbol t = begin
    match t.token.typ with
    | Token.SYMBOL ident ->
      next t;
      begin match t.token.typ with
      | Token.LEFT_PAREN ->
        t |> expect Token.LEFT_PAREN;
        let attrs = t |> Helpers.separated_list ~fn:attr ~sep:Token.COMMA in
        t |> expect Token.RIGHT_PAREN;
        Some (Ast_Symbol.make ident attrs)
      | _ -> None
      end
    | _ -> None
  end
end

let scan t = begin
  let start_pos = t.token.start_pos in
  let symbols = t |> Helpers.list ~fn:Rules.symbol in
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