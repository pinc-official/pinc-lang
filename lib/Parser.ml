exception Parser_Error of string

type t = {
  mutable lexer: Lexer.t;
  mutable token: Token.t;
  mutable prev_token: Token.t option;
}

let (let*) = Option.bind

let rec next ?prev_token t = begin
  let prev_token = prev_token |> Option.value ~default:t.token in
  let token = Lexer.scan t.lexer in
  match token.typ with
  | Token.COMMENT -> next t
  | _             ->
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
    print_endline (Printf.sprintf "%i:%i" t.token.start_pos.line t.token.start_pos.column);
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
          else (
            print_endline (Printf.sprintf "%i:%i" t.token.start_pos.line t.token.start_pos.column);
            raise (Parser_Error (Printf.sprintf "Expected list to be separated by %s" (Token.to_string sep)))
          )
      | None   -> List.rev acc
    end in
    loop []
  end


  let non_empty_separated_list ~sep ~fn t = begin
    let res = separated_list ~sep ~fn t in
    if res = [] then (
      print_endline (Printf.sprintf "%i:%i" t.token.start_pos.line t.token.start_pos.column);
      raise (Parser_Error (Printf.sprintf "Expected list to not be empty"))
    );
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
  let rec expect_identifier ?(typ=`All) t = begin
    match t.token.typ with
    | Token.IDENT_UPPER i when typ = `Upper || typ = `All -> next t; i
    | Token.IDENT_LOWER i when typ = `Lower || typ = `All -> next t; i
    | _ -> assert false (* TODO: Error message *)
  end

  and literal t = begin
    match t.token.typ with
    | Token.STRING s      -> next t; Some (Ast.StringLiteral s)
    | Token.INT i         -> next t; Some (Ast.IntLiteral i)
    | Token.FLOAT f       -> next t; Some (Ast.FloatLiteral f)
    | Token.KEYWORD_TRUE  -> next t; Some (Ast.BoolLiteral true)
    | Token.KEYWORD_FALSE -> next t; Some (Ast.BoolLiteral false)
    | _ -> None
  end

  and attribute ?(sep=Token.COLON) t = begin
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      expect sep t;
      let value = expr t in
      value |> Option.map (fun value ->
        Ast.{ key; value }
      )
    | _ -> None
  end

  and template_node t = begin
    match t.token.typ with
    | Token.STRING s ->
      next t;
      Some (Ast.TextTemplateNode s)
    | Token.LEFT_BRACE ->
      next t;
      (* t.lexer |> Lexer.setNormalMode; *)
      let expression = match expr t with
      | Some e -> Ast.ExpressionTemplateNode e
      | None   -> assert false; (* TODO: Error Message *)
      in
      (* t.lexer |> Lexer.popNormalMode; *)
      t |> expect Token.RIGHT_BRACE;
      Some expression
    | Token.HTML_OPEN_TAG tag -> begin
      next t;
      let attributes = Helpers.list ~fn:(attribute ~sep:Token.EQUAL) t in
      let self_closing = t |> optional Token.SLASH in
      t |> expect Token.GREATER;
      let children = if self_closing
        then []
        else (
          let children = t |> Helpers.list ~fn:template_node in
          t |> expect (Token.HTML_CLOSE_TAG tag);
          children
        )
      in
      Some (Ast.HtmlTemplateNode { tag; self_closing; attributes; children; })
    end
    | Token.LESS ->
      next t;
      let node = match t.token.typ with
      | Token.IDENT_UPPER name -> begin
        next t;
        let attributes = Helpers.list ~fn:(attribute ~sep:Token.EQUAL) t in
        let self_closing = t |> optional Token.SLASH in
        t |> expect Token.GREATER;
        let children = if self_closing
          then []
          else (
            let children = t |> Helpers.list ~fn:template_node in
            t |> expect Token.LESS_SLASH;
            t |> expect (Token.IDENT_LOWER name);
            t |> expect Token.GREATER;
            children
          )
        in
        t |> expect Token.LESS_SLASH;
        t |> expect (Token.IDENT_LOWER name);
        t |> expect Token.GREATER;
        Ast.ComponentTemplateNode { identifier = Id name; self_closing; attributes; children; }
      end
      | _ -> assert false (* TODO: Error Message *)
      in
      Some node
    | _ -> None
  end

  and expr t = begin
    match t.token.typ with
    | Token.LEFT_BRACK -> begin
        next t;
        let expressions = Helpers.separated_list ~sep:Token.COMMA ~fn:expr t in
        expect Token.RIGHT_BRACK t;
        Some (Ast.ArrayExpression expressions)
      end
    | Token.SYMBOL name -> begin
        next t;
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

        Some (Ast.SymbolExpression { name; attributes; body; })
      end
    | Token.HTML_OPEN_TAG _ ->
      let template_nodes = t |> Helpers.list ~fn:template_node in
      Some (Ast.TemplateExpression template_nodes)
    | Token.TEMPLATE ->
      next t;
      t.lexer |> Lexer.setTemplateBlockMode;
      t |> expect Token.LEFT_BRACE;
      let template_nodes = t |> Helpers.list ~fn:template_node in
      t.lexer |> Lexer.popTemplateBlockMode;
      t |> expect Token.RIGHT_BRACE;
      Some (Ast.TemplateExpression template_nodes)
    | Token.KEYWORD_IF -> begin
        next t;

        expect Token.LEFT_PAREN t;
        let* condition = expr t in
        expect Token.RIGHT_PAREN t;

        let* consequent = expr t in

        let alternate = 
          if optional Token.KEYWORD_ELSE t
          then expr t
          else None
        in

        Some (Ast.ConditionalExpression {
          condition;
          consequent;
          alternate;
        })
      end
    | (Token.NOT | Token.MINUS | Token.PLUS) as o -> begin
      let operator = match o with
      | Token.NOT   -> Ast.Operator.NOT
      | Token.MINUS -> Ast.Operator.NEGATIVE
      | _ -> assert false
      in
      next t;
      let argument = expr t in
      argument |> Option.map (fun argument ->
        Ast.UnaryExpression { operator; argument; }
      )
      end
    | Token.LEFT_BRACE ->
      next t;
      let statements = Helpers.list ~fn:statement t in
      t |> expect Token.RIGHT_BRACE;
      Some (Ast.BlockExpression statements)
    | Token.KEYWORD_FOR -> begin
      next t;
      match t.token.typ with
      | Token.IDENT_LOWER identifier ->
        next t;
        let* right = expr t in
        let body = Helpers.list ~fn:statement t in
        let left = Ast.Id identifier in
        Some (Ast.ForInExpression { left; right; body; })
      | _ -> assert false (* TODO: Error Message :) *)
    end
    | Token.IDENT_LOWER identifier
    | Token.IDENT_UPPER identifier ->
      next t;
      Some (Ast.IdentifierExpression (Id identifier))
    | Token.STRING _
    | Token.INT _
    | Token.FLOAT _
    | Token.KEYWORD_TRUE 
    | Token.KEYWORD_FALSE -> begin
        let literal = literal t in
        literal |> Option.map (fun o ->
          Ast.LiteralExpression o
        )
      end
    | _ -> None
  end

  and statement t = begin
    match t.token.typ with
    | Token.KEYWORD_BREAK ->
      next t;
      Some Ast.BreakStmt
    | Token.KEYWORD_CONTINUE ->
      next t;
      Some Ast.ContinueStmt
    | Token.IDENT_LOWER identifier -> begin
      next t;
      let nullable = t |> optional Token.QUESTIONMARK in
      t |> expect Token.EQUAL;
      let expression = expr t in
      expect Token.SEMICOLON t;
      match expression with
      | Some right -> Some (Ast.DeclarationStmt { nullable; left = Id identifier; right })
      | None       -> assert false (* TODO: Exception *)
    end
    | _ ->
      match expr t with
      | Some expr -> Some (Ast.ExpressionStmt expr)
      | None      -> None
  end

  and declaration t = begin
    match t.token.typ with
    | (Token.KEYWORD_SITE | Token.KEYWORD_PAGE | Token.KEYWORD_COMPONENT | Token.KEYWORD_STORE) as typ -> begin
        let start_pos = t.token.start_pos in
        next t;
        let identifier = expect_identifier ~typ:`Upper t in
        let identifier = Ast.Id identifier in
        let attributes = if optional Token.LEFT_PAREN t then
          let attributes = Helpers.separated_list ~sep:Token.COMMA ~fn:attribute t in
          t |> expect Token.RIGHT_PAREN;
          Some attributes
        else
          None
        in
        let body = Helpers.non_empty_list ~fn:statement t in
        match typ with
        | Token.KEYWORD_SITE      -> Some (Ast.SiteDeclaration      { location = start_pos; identifier; attributes; body })
        | Token.KEYWORD_PAGE      -> Some (Ast.PageDeclaration      { location = start_pos; identifier; attributes; body })
        | Token.KEYWORD_COMPONENT -> Some (Ast.ComponentDeclaration { location = start_pos; identifier; attributes; body })
        | Token.KEYWORD_STORE     -> Some (Ast.StoreDeclaration     { location = start_pos; identifier; attributes; body })
        | _ -> assert false
      end
    | Token.END_OF_INPUT -> None
    | _ -> assert false
  end
end

let scan t = begin
  let declarations = t |> Helpers.list ~fn:Rules.declaration in

  declarations
end