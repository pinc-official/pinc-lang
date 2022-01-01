exception Parser_Error of string

type t = {
  mutable lexer: Lexer.t;
  mutable token: Token.t;
  mutable prev_token: Token.t option;
}

let (let*) = Option.bind

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
  let rec literal t = begin
    match t.token.typ with
    | Token.STRING s      -> next t; Some (Ast.Literal.String s)
    | Token.INT i         -> next t; Some (Ast.Literal.Int i)
    | Token.FLOAT f       -> next t; Some (Ast.Literal.Float f)
    | Token.KEYWORD_TRUE  -> next t; Some (Ast.Literal.Bool true)
    | Token.KEYWORD_FALSE -> next t; Some (Ast.Literal.Bool false)
    | _ -> None
  end

  and expr t = begin
    match t.token.typ with
    | Token.LEFT_BRACK -> begin
        next t;
        let expressions = Helpers.separated_list ~sep:Token.COMMA ~fn:expr t in
        expect Token.RIGHT_BRACK t;
        Some (Ast.Expression.Array expressions)
      end
    | Token.SYMBOL _ -> begin
        let symbols = Helpers.list ~fn:symbol t in
        Some (Ast.Expression.Symbols symbols)
      end
    | Token.KEYWORD_IF -> begin
        next t;

        expect Token.LEFT_PAREN t;
        let* condition = expr t in
        expect Token.RIGHT_PAREN t;
        
        expect Token.LEFT_BRACE t;
        let* consequent = expr t in
        expect Token.RIGHT_BRACE t;

        let alternate = if optional Token.KEYWORD_ELSE t
          then expr t
          else None
        in

        Some (Ast.Expression.Conditional {
          condition;
          consequent;
          alternate;
        })
      end
    | (Token.NOT | Token.MINUS | Token.PLUS) as o -> begin
      let operator = match o with
      | Token.NOT   -> Ast.Operator.NOT
      | Token.MINUS -> Ast.Operator.NEGATIVE
      | Token.PLUS  -> Ast.Operator.POSITIVE
      | _ -> assert false
      in
      next t;
      let argument = expr t in
      argument |> Option.map (fun argument ->
        Ast.Expression.Unary { operator; argument; }
      )
      end
    | Token.STRING _
    | Token.INT _
    | Token.FLOAT _
    | Token.KEYWORD_TRUE 
    | Token.KEYWORD_FALSE -> begin
        let literal = literal t in
        literal |> Option.map (fun o ->
          Ast.Expression.Literal o
        )
      end
    | Token.IDENT_LOWER left -> begin
        next t;
        expect Token.EQUAL t;
        let* right = expr t in
        expect Token.SEMICOLON t;
        Some (Ast.Expression.Assignment { left; right })
      end
    | _ -> None
  end

  and statement t = begin
    match t.token.typ with
    | Token.KEYWORD_BREAK ->
      next t; 
      Some Ast.Statement.Break
    | Token.KEYWORD_CONTINUE ->
      next t; 
      Some Ast.Statement.Continue
    | Token.LEFT_BRACE ->
      next t;
      let statements = Helpers.list ~fn:statement t in
      t |> expect Token.RIGHT_BRACE;
      Some (Ast.Statement.Block statements)
    | Token.KEYWORD_FOR -> begin
      next t;
      match t.token.typ with
      | Token.IDENT_LOWER left ->
        next t;
        let* right = expr t in
        let body = Helpers.list ~fn:statement t in
        Some (Ast.Statement.ForIn { left; right; body; })
      | _ -> assert false (* TODO: Error Message :) *)
      end
    | _ ->
      match expr t with
      | Some expr -> Some (Ast.Statement.Expression expr)
      | None      -> None
  end

  and attribute t = begin
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      expect Token.COLON t;
      let value = expr t in
      value |> Option.map (fun value ->
        Ast.Attribute.{ key; value }
      )
    | _ -> None
  end
  
  and symbol t = begin
    match t.token.typ with
    | Token.SYMBOL identifier ->
      next t;
      begin match t.token.typ with
      | Token.LEFT_PAREN ->
        t |> expect Token.LEFT_PAREN;
        let attributes = t |> Helpers.separated_list ~fn:attribute ~sep:Token.COMMA in
        t |> expect Token.RIGHT_PAREN;
        let body = if t |> optional Token.LEFT_BRACE then (
          let body = statement t in
          t |> expect Token.RIGHT_BRACE;
          body
        ) else (
          None
        ) in
        Some Ast.Symbol.{ identifier; attributes; body }
      | _ -> None
      end
    | _ -> None
  end

  and declaration ~attributes t = begin
    match t.token.typ with
    | Token.KEYWORD_SITE -> begin
      next t;
      match t.token.typ with
      | Token.IDENT_UPPER identifier -> begin
          next t;
          let* body = statement t in
          Some (Ast.Declaration.Site { identifier; attributes; body })
        end
      | _ -> assert false (* TODO: Error Message *)
      end
    | Token.KEYWORD_PAGE -> begin
      next t;
      match t.token.typ with
      | Token.IDENT_UPPER identifier -> begin
          next t;
          let* body = statement t in
          Some (Ast.Declaration.Page { identifier; attributes; body })
        end
      | _ -> assert false (* TODO: Error Message *)
      end
    | Token.KEYWORD_COMPONENT -> begin
      next t;
      match t.token.typ with
      | Token.IDENT_UPPER identifier -> begin
          next t;
          let* body = statement t in
          Some (Ast.Declaration.Component { identifier; attributes; body })
        end
      | _ -> assert false (* TODO: Error Message *)
      end
    | Token.KEYWORD_STORE -> begin
      next t;
      match t.token.typ with
      | Token.IDENT_UPPER identifier -> begin
          next t;
          let* body = statement t in
          Some (Ast.Declaration.Store { identifier; attributes; body })
        end
      | _ -> assert false (* TODO: Error Message *)
      end
    | _ -> None
  end
end

let scan t = begin
  let start_pos = t.token.start_pos in
  let attributes = t |> Helpers.list ~fn:Rules.symbol in
  let declarations = t |> Helpers.list ~fn:(Rules.declaration ~attributes) in

  Ast.File.{location = start_pos; declarations}
end