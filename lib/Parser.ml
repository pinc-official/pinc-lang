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
  let expect_identifier ?(typ=`All) t = begin
    let start_pos = Lexer.make_position t.lexer in
    match t.token.typ with
    | Token.IDENT_UPPER i when typ = `Upper || typ = `All -> next t; i
    | Token.IDENT_LOWER i when typ = `Lower || typ = `All -> next t; i
    | token when typ = `Lower ->
      Diagnostics.report ~start_pos ~end_pos:(Lexer.make_position t.lexer) (Diagnostics.ExpectedLowerIdent token)
    | token when typ = `Upper ->
      Diagnostics.report ~start_pos ~end_pos:(Lexer.make_position t.lexer) (Diagnostics.ExpectedUpperIdent token)
    | token ->
      Diagnostics.report ~start_pos ~end_pos:(Lexer.make_position t.lexer) (Diagnostics.ExpectedIdent token)
  end

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
  let rec parse_literal t = begin
    match t.token.typ with
    | Token.STRING s      -> next t; Some (Ast.StringLiteral s)
    | Token.INT i         -> next t; Some (Ast.IntLiteral i)
    | Token.FLOAT f       -> next t; Some (Ast.FloatLiteral f)
    | Token.KEYWORD_TRUE  -> next t; Some (Ast.BoolLiteral true)
    | Token.KEYWORD_FALSE -> next t; Some (Ast.BoolLiteral false)
    | _ -> None
  end

  and parse_attribute ?(sep=Token.COLON) t = begin
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      expect sep t;
      let value = parse_expression t in
      value |> Option.map (fun value ->
        Ast.{ key; value }
      )
    | _ -> None
  end

  and parse_template_node t = begin
    match t.token.typ with
    | Token.STRING s ->
      next t;
      Some (Ast.TextTemplateNode s)
    | Token.LEFT_BRACE ->
      next t;
      let expression = match parse_expression t with
      | Some e -> Ast.ExpressionTemplateNode e
      | None   -> assert false; (* TODO: Error Message *)
      in
      t |> expect Token.RIGHT_BRACE;
      Some expression
    | Token.HTML_OPEN_TAG tag -> begin
      next t;
      let attributes = Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL) t in
      let self_closing = t |> optional Token.SLASH in
      t |> expect Token.GREATER;
      let children = if self_closing
        then []
        else (
          let children = t |> Helpers.list ~fn:parse_template_node in
          t |> expect (Token.HTML_CLOSE_TAG tag);
          children
        )
      in
      Some (Ast.HtmlTemplateNode { tag; attributes; children; })
    end
    | Token.COMPONENT_OPEN_TAG identifier -> begin
      next t;
      let attributes = Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL) t in
      let self_closing = t |> optional Token.SLASH in
      t |> expect Token.GREATER;
      let children = if self_closing
        then []
        else (
          let children = t |> Helpers.list ~fn:parse_template_node in
          t |> expect (Token.COMPONENT_CLOSE_TAG identifier);
          children
        )
      in
      Some (Ast.ComponentTemplateNode { identifier = Id identifier; attributes; children; })
    end
    | _ -> None
  end

  and parse_expression_part t = begin
    match t.token.typ with
    (* PARSING BLOCK EXPRESSION *)
    | Token.LEFT_BRACE -> begin
      next t;
      let statements = Helpers.list ~fn:parse_statement t in
      t |> expect Token.RIGHT_BRACE;
      Some (Ast.BlockExpression statements)
    end

    (* PARSING FOR IN EXPRESSION *)
    | Token.KEYWORD_FOR -> begin
      next t;
      expect Token.LEFT_PAREN t;
      let identifier = Helpers.expect_identifier ~typ:`Lower t in
      let iterator = Ast.Id identifier in
      expect Token.KEYWORD_IN t;
      let* expr1 = parse_expression t in
      if optional Token.KEYWORD_TO t then (
        let* expr2 = parse_expression t in
        expect Token.RIGHT_PAREN t;
        let body = Helpers.list ~fn:parse_statement t in
        Some (Ast.ForInRangeExpression { iterator; from=expr1; upto=expr2; body; })
      ) else (
        expect Token.RIGHT_PAREN t;
        let body = Helpers.list ~fn:parse_statement t in
        Some (Ast.ForInExpression { iterator; iterable=expr1; body; })
      )

    end

    (* PARSING IF EXPRESSION *)
    | Token.KEYWORD_IF -> begin
      next t;
      expect Token.LEFT_PAREN t;
      let* condition = parse_expression t in
      expect Token.RIGHT_PAREN t;
      let* consequent = parse_expression t in
      let alternate = 
        if optional Token.KEYWORD_ELSE t
        then parse_expression t
        else None
      in
      Some (Ast.ConditionalExpression {
        condition;
        consequent;
        alternate;
      })
    end

    (* PARSING ARRAY EXPRESSION *)
    | Token.LEFT_BRACK -> begin
        next t;
        let expressions = Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression t in
        expect Token.RIGHT_BRACK t;
        Some (Ast.ArrayExpression expressions)
      end

    (* PARSING TAG EXPRESSION *)
    | Token.TAG name -> begin
      next t;
      t |> expect Token.LEFT_PAREN;
      let attributes = t |> Helpers.separated_list ~fn:parse_attribute ~sep:Token.COMMA in
      t |> expect Token.RIGHT_PAREN;

      let body = if t |> optional Token.LEFT_BRACE then (
        let body = parse_statement t in
        t |> expect Token.RIGHT_BRACE;
        body
      ) else (
        None
      ) in

      Some (Ast.TagExpression { name; attributes; body; })
    end

    (* PARSING TEMPLATE EXPRESSION *)
    | Token.HTML_OPEN_TAG _
    | Token.COMPONENT_OPEN_TAG _ -> begin
      let template_nodes = t |> Helpers.list ~fn:parse_template_node in
      Some (Ast.TemplateExpression template_nodes)
    end

    (* PARSING TEMPLATE EXPRESSION *)
    | Token.TEMPLATE ->
      next t;
      t.lexer |> Lexer.setTemplateBlockMode;
      t |> expect Token.LEFT_BRACE;
      let template_nodes = t |> Helpers.list ~fn:parse_template_node in
      t.lexer |> Lexer.popTemplateBlockMode;
      t |> expect Token.RIGHT_BRACE;
      Some (Ast.TemplateExpression template_nodes)

    (* PARSING UNARY EXPRESSION *)
    | (Token.NOT | Token.UNARY_MINUS) as o -> begin
      let operator = match o with
      | Token.NOT         -> Ast.Operator.NOT
      | Token.UNARY_MINUS -> Ast.Operator.NEGATIVE
      | _ -> assert false
      in
      next t;
      let argument = parse_expression t in
      argument |> Option.map (fun argument ->
        Ast.UnaryExpression { operator; argument; }
      )
      end

    (* PARSING IDENTIFIER EXPRESSION *)
    | Token.IDENT_LOWER identifier
    | Token.IDENT_UPPER identifier ->
      next t;
      Some (Ast.IdentifierExpression (Id identifier))

    (* PARSING LITERAL EXPRESSION *)
    | Token.STRING _
    | Token.INT _
    | Token.FLOAT _
    | Token.KEYWORD_TRUE 
    | Token.KEYWORD_FALSE -> begin
        let literal = parse_literal t in
        literal |> Option.map (fun o ->
          Ast.LiteralExpression o
        )
      end

    | _ -> None
  end

  and parse_binary_operator t = begin
    match t.token.typ with
    | Token.LOGICAL_AND -> next t; Some Ast.Operator.AND
    | Token.LOGICAL_OR  -> next t; Some Ast.Operator.OR

    | Token.EQUAL_EQUAL -> next t; Some Ast.Operator.EQUAL
    | Token.NOT_EQUAL -> next t; Some Ast.Operator.NOT_EQUAL

    | Token.GREATER -> next t; Some Ast.Operator.GREATER
    | Token.GREATER_EQUAL -> next t; Some Ast.Operator.GREATER_EQUAL
    | Token.LESS -> next t; Some Ast.Operator.LESS
    | Token.LESS_EQUAL -> next t; Some Ast.Operator.LESS_EQUAL

    | Token.PLUS -> next t; Some Ast.Operator.PLUS
    | Token.MINUS -> next t; Some Ast.Operator.MINUS
    | Token.STAR -> next t; Some Ast.Operator.TIMES
    | Token.SLASH -> next t; Some Ast.Operator.DIV
    | Token.STAR_STAR -> next t; Some Ast.Operator.POW

    | Token.KEYWORD_IN -> next t; Some Ast.Operator.IN

    | _ -> None
  end

  and parse_expression t = begin
    let* left = parse_expression_part t in
    (* TODO: 
      Mhmm...is this the best way to do this?
      We are only looking for binary operators when we are not in template mode.
      This is to ensure, we are not parsing the end of a html tag <div> as a binary operator (GREATER).
      Maybe we can also do this in the lexer...
    *)
    if Lexer.inNormalMode t.lexer then
      match parse_binary_operator t with
      | Some operator ->
        begin
          let right = parse_expression_part t in
          match right with
          | Some right -> Some (Ast.BinaryExpression { left; operator; right })
          | None       -> failwith "Expected expression"
        end
      | None -> Some left
    else
      Some left
  end

  and parse_statement t = begin
    match t.token.typ with
    | Token.KEYWORD_BREAK ->
      next t;
      Some Ast.BreakStmt
    | Token.KEYWORD_CONTINUE ->
      next t;
      Some Ast.ContinueStmt
    | Token.KEYWORD_LET -> begin
      next t;
      let identifier = Helpers.expect_identifier t in
      let nullable = t |> optional Token.QUESTIONMARK in
      t |> expect Token.EQUAL;
      let expression = parse_expression t in
      expect Token.SEMICOLON t;
      match expression with
      | Some right -> Some (Ast.DeclarationStmt { nullable; left = Id identifier; right })
      | None       -> assert false (* TODO: Exception *)
    end
    | _ ->
      match parse_expression t with
      | Some expr -> Some (Ast.ExpressionStmt expr)
      | None      -> None
  end

  and parse_declaration t = begin
    match t.token.typ with
    | (Token.KEYWORD_SITE | Token.KEYWORD_PAGE | Token.KEYWORD_COMPONENT | Token.KEYWORD_STORE) as typ -> begin
        let start_pos = t.token.start_pos in
        next t;
        let identifier = Helpers.expect_identifier ~typ:`Upper t in
        let identifier = Ast.Id identifier in
        let attributes = if optional Token.LEFT_PAREN t then
          let attributes = Helpers.separated_list ~sep:Token.COMMA ~fn:parse_attribute t in
          t |> expect Token.RIGHT_PAREN;
          Some attributes
        else
          None
        in
        let body = Helpers.non_empty_list ~fn:parse_statement t in
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
  let declarations = t |> Helpers.list ~fn:Rules.parse_declaration in

  declarations
end
