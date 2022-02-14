exception Parser_Error of string

type t =
  { mutable lexer : Lexer.t
  ; mutable token : Token.t
  ; next : Token.t Queue.t
  }

let ( let* ) = Option.bind

let get_next_token t =
  match Queue.take_opt t.next with
  | None -> Lexer.scan t.lexer
  | Some token -> token
;;

let rec next t =
  let token = get_next_token t in
  match token.typ with
  | Token.COMMENT -> next t
  | _ -> t.token <- token
;;

let rec peek t =
  let token = get_next_token t in
  match token.typ with
  | Token.COMMENT -> peek t
  | typ ->
    Queue.add token t.next;
    typ
;;

let optional token t =
  let test = t.token.typ = token in
  if test then next t;
  test
;;

let expect token t =
  let test = t.token.typ = token in
  if test
  then next t
  else (
    print_endline (Printf.sprintf "%i:%i" t.token.start_pos.line t.token.start_pos.column);
    raise
      (Parser_Error
         (Printf.sprintf
            "Expected: %s, got %s"
            (Token.to_string token)
            (Token.to_string t.token.typ))))
;;

let make ~filename src =
  let lexer = Lexer.make ~filename src in
  let initial_pos = Position.make ~filename ~line:0 ~column:0 in
  let initial_token =
    Token.make ~start_pos:initial_pos ~end_pos:initial_pos Token.END_OF_INPUT
  in
  let t = { lexer; token = initial_token; next = Queue.create () } in
  next t;
  t
;;

module Helpers = struct
  let expect_identifier ?(typ = `All) t =
    let start_pos = Lexer.make_position t.lexer in
    match t.token.typ with
    | Token.IDENT_UPPER i when typ = `Upper || typ = `All ->
      next t;
      i
    | Token.IDENT_LOWER i when typ = `Lower || typ = `All ->
      next t;
      i
    | token when typ = `Lower ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(Lexer.make_position t.lexer)
        (Diagnostics.ExpectedLowerIdent token)
    | token when typ = `Upper ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(Lexer.make_position t.lexer)
        (Diagnostics.ExpectedUpperIdent token)
    | token ->
      Diagnostics.report
        ~start_pos
        ~end_pos:(Lexer.make_position t.lexer)
        (Diagnostics.ExpectedIdent token)
  ;;

  let separated_list ~sep ~fn t =
    let rec loop acc =
      let has_sep = optional sep t in
      match fn t with
      | Some r ->
        if has_sep || Iter.is_empty acc
        then loop (Iter.cons r acc)
        else (
          print_endline
            (Printf.sprintf "%i:%i" t.token.start_pos.line t.token.start_pos.column);
          raise
            (Parser_Error
               (Printf.sprintf
                  "Expected list to be separated by %s"
                  (Token.to_string sep))))
      | None -> Iter.rev acc
    in
    loop Iter.empty
  ;;

  let non_empty_separated_list ~sep ~fn t =
    let res = separated_list ~sep ~fn t in
    if Iter.is_empty res
    then (
      print_endline
        (Printf.sprintf "%i:%i" t.token.start_pos.line t.token.start_pos.column);
      raise (Parser_Error (Printf.sprintf "Expected list to not be empty")));
    res
  ;;

  let list ~fn t =
    let rec loop acc =
      match fn t with
      | Some r -> loop (Iter.cons r acc)
      | None -> Iter.rev acc
    in
    loop Iter.empty
  ;;

  let non_empty_list ~fn t =
    let res = list ~fn t in
    if Iter.is_empty res
    then raise (Parser_Error (Printf.sprintf "Expected list to not be empty"));
    res
  ;;
end

module Rules = struct
  let rec parse_literal t =
    match t.token.typ with
    | Token.STRING s ->
      next t;
      Some (Ast.Literal.String s)
    | Token.INT i ->
      next t;
      Some (Ast.Literal.Int i)
    | Token.FLOAT f ->
      next t;
      Some (Ast.Literal.Float f)
    | Token.KEYWORD_TRUE ->
      next t;
      Some (Ast.Literal.Bool true)
    | Token.KEYWORD_FALSE ->
      next t;
      Some (Ast.Literal.Bool false)
    | _ -> None

  and parse_tag name t =
    next t;
    let attributes =
      if t |> optional Token.LEFT_PAREN
      then (
        let res = t |> Helpers.separated_list ~fn:parse_attribute ~sep:Token.COMMA in
        t |> expect Token.RIGHT_PAREN;
        res)
      else Iter.empty
    in
    let get_attr key (attr_key, attr_value) =
      if attr_key = key then Some attr_value else None
    in
    match name with
    | "String" ->
      Some
        (Ast.TagString
           { label = Iter.find (get_attr "label") attributes
           ; placeholder = Iter.find (get_attr "placeholder") attributes
           ; inline = Iter.find (get_attr "inline") attributes
           ; default_value = Iter.find (get_attr "defaultValue") attributes
           })
    | "Int" ->
      Some
        (Ast.TagInt
           { label = Iter.find (get_attr "label") attributes
           ; placeholder = Iter.find (get_attr "placeholder") attributes
           ; default_value = Iter.find (get_attr "defaultValue") attributes
           })
    | "Float" ->
      Some
        (Ast.TagFloat
           { label = Iter.find (get_attr "label") attributes
           ; placeholder = Iter.find (get_attr "placeholder") attributes
           ; default_value = Iter.find (get_attr "defaultValue") attributes
           })
    | "Boolean" ->
      Some
        (Ast.TagBoolean
           { label = Iter.find (get_attr "label") attributes
           ; default_value = Iter.find (get_attr "defaultValue") attributes
           })
    | "Array" ->
      Some
        (Ast.TagArray
           { label = Iter.find (get_attr "label") attributes
           ; default_value = Iter.find (get_attr "defaultValue") attributes
           ; elements =
               (match Iter.find (get_attr "of") attributes with
               | Some (Ast.TagExpression (tag, body)) -> tag, body
               | Some _ -> failwith "expected attribute `of` on array tag, to be a tag."
               | None ->
                 failwith
                   "expected attribute `of` on array tag, to describe the type of the \
                    elements inside the array.")
           })
    | "Record" ->
      Some
        (Ast.TagRecord
           { label = Iter.find (get_attr "label") attributes
           ; properties =
               (match Iter.find (get_attr "of") attributes with
               | Some (Ast.RecordExpression attributes) ->
                 attributes
                 |> Iter.map (fun (key, value) ->
                        match value with
                        | Ast.TagExpression (tag, body) -> key, tag, body
                        | _ ->
                          failwith
                            ("expected property `" ^ key ^ "` on record tag, to be a tag."))
               | Some _ ->
                 failwith "expected attribute `of` on record tag, to be a record of tags."
               | None ->
                 failwith
                   "expected attribute `of` on record tag, to describe the type of the \
                    properties inside the record.")
           })
    | s -> failwith ("Unknown tag with name `" ^ s ^ "`.")

  and parse_attribute ?(sep = Token.COLON) t =
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      expect sep t;
      let value = parse_expression t in
      value |> Option.map (fun value -> key, value)
    | _ -> None

  and parse_template_node t =
    match t.token.typ with
    | Token.STRING s ->
      next t;
      Some (Ast.TextTemplateNode s)
    | Token.LEFT_BRACE ->
      next t;
      let expression =
        match parse_expression t with
        | Some e -> Ast.ExpressionTemplateNode e
        | None ->
          (* TODO: Should this be an error? *)
          ExpressionTemplateNode (Ast.LiteralExpression Ast.Literal.Null)
      in
      t |> expect Token.RIGHT_BRACE;
      Some expression
    | Token.HTML_OPEN_TAG tag ->
      next t;
      let attributes = Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL) t in
      let self_closing = t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING in
      let children =
        if self_closing
        then Iter.empty
        else (
          t |> expect Token.HTML_OR_COMPONENT_TAG_END;
          let children = t |> Helpers.list ~fn:parse_template_node in
          t |> expect (Token.HTML_CLOSE_TAG tag);
          children)
      in
      Some (Ast.HtmlTemplateNode { tag; attributes; children; self_closing })
    | Token.COMPONENT_OPEN_TAG identifier ->
      next t;
      let attributes = Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL) t in
      let self_closing = t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING in
      let children =
        if self_closing
        then Iter.empty
        else (
          t |> expect Token.HTML_OR_COMPONENT_TAG_END;
          let children = t |> Helpers.list ~fn:parse_template_node in
          t |> expect (Token.COMPONENT_CLOSE_TAG identifier);
          children)
      in
      Some
        (Ast.ComponentTemplateNode { identifier = Id identifier; attributes; children })
    | _ -> None

  and parse_expression_part t =
    match t.token.typ with
    (* PARSING PARENTHESIZED EXPRESSION *)
    | Token.LEFT_PAREN ->
      next t;
      let expr = parse_expression t in
      t |> expect Token.RIGHT_PAREN;
      expr
    (* PARSING RECORD or BLOCK EXPRESSION *)
    | Token.LEFT_BRACE ->
      next t;
      let is_record =
        match t.token.typ with
        | Token.IDENT_LOWER _ -> peek t = Token.COLON
        | _ -> false
      in
      if is_record
      then (
        let attrs = Helpers.separated_list ~sep:Token.COMMA ~fn:parse_attribute t in
        t |> expect Token.RIGHT_BRACE;
        Some (Ast.RecordExpression attrs))
      else (
        let statements = Helpers.list ~fn:parse_statement t in
        t |> expect Token.RIGHT_BRACE;
        Some (Ast.BlockExpression statements))
    (* PARSING FOR IN EXPRESSION *)
    | Token.KEYWORD_FOR ->
      next t;
      expect Token.LEFT_PAREN t;
      let identifier = Helpers.expect_identifier ~typ:`Lower t in
      let iterator = Ast.Id identifier in
      expect Token.KEYWORD_IN t;
      let reverse = optional Token.KEYWORD_REVERSE t in
      let* expr1 = parse_expression t in
      let exclusive_range = t |> optional Token.DOTDOT in
      let inclusive_range = t |> optional Token.DOTDOTDOT in
      if exclusive_range || inclusive_range
      then (
        let* expr2 = parse_expression t in
        expect Token.RIGHT_PAREN t;
        let body = Helpers.list ~fn:parse_statement t in
        Some
          (Ast.ForInRangeExpression
             { iterator
             ; reverse
             ; from = expr1
             ; upto = expr2
             ; inclusive = inclusive_range
             ; body
             }))
      else (
        expect Token.RIGHT_PAREN t;
        let body = Helpers.list ~fn:parse_statement t in
        Some (Ast.ForInExpression { iterator; reverse; iterable = expr1; body }))
    (* PARSING IF EXPRESSION *)
    | Token.KEYWORD_IF ->
      next t;
      expect Token.LEFT_PAREN t;
      let* condition = parse_expression t in
      expect Token.RIGHT_PAREN t;
      let* consequent = parse_expression t in
      let alternate =
        if optional Token.KEYWORD_ELSE t then parse_expression t else None
      in
      Some (Ast.ConditionalExpression { condition; consequent; alternate })
    (* PARSING ARRAY EXPRESSION *)
    | Token.LEFT_BRACK ->
      next t;
      let expressions = Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression t in
      expect Token.RIGHT_BRACK t;
      Some (Ast.ArrayExpression expressions)
    (* PARSING TAG EXPRESSION *)
    | Token.TAG name ->
      let* tag = parse_tag name t in
      let transformer =
        if t |> optional Token.DOUBLE_COLON
        then (
          let bind = t |> Helpers.expect_identifier ~typ:`Lower in
          t |> expect Token.ARROW;
          let body =
            match parse_expression t with
            | None -> failwith "Expected expression as transformer."
            | Some expr -> expr
          in
          Some (Ast.Id bind, body))
        else None
      in
      Some (Ast.TagExpression (tag, transformer))
    (* PARSING TEMPLATE EXPRESSION *)
    | Token.HTML_OPEN_TAG _ | Token.COMPONENT_OPEN_TAG _ ->
      let template_nodes = t |> Helpers.list ~fn:parse_template_node in
      Some (Ast.TemplateExpression template_nodes)
    (* PARSING TEMPLATE EXPRESSION *)
    | Token.TEMPLATE ->
      next t;
      t |> expect Token.LEFT_BRACE;
      let template_nodes = t |> Helpers.list ~fn:parse_template_node in
      t |> expect Token.RIGHT_BRACE;
      Some (Ast.TemplateExpression template_nodes)
    (* PARSING UNARY NOT EXPRESSION *)
    | Token.NOT ->
      next t;
      let operator = Ast.Operators.Unary.make NOT in
      let* argument = parse_expression ~prio:operator.precedence t in
      Some (Ast.UnaryExpression { operator = Ast.Operators.Unary.NOT; argument })
    (* PARSING UNARY MINUS EXPRESSION *)
    | Token.UNARY_MINUS ->
      next t;
      let operator = Ast.Operators.Unary.make MINUS in
      let* argument = parse_expression ~prio:operator.precedence t in
      Some (Ast.UnaryExpression { operator = Ast.Operators.Unary.MINUS; argument })
    (* PARSING IDENTIFIER EXPRESSION *)
    | Token.IDENT_LOWER identifier | Token.IDENT_UPPER identifier ->
      next t;
      Some (Ast.IdentifierExpression (Id identifier))
    (* PARSING LITERAL EXPRESSION *)
    | Token.STRING _
    | Token.INT _
    | Token.FLOAT _
    | Token.KEYWORD_TRUE
    | Token.KEYWORD_FALSE ->
      let literal = parse_literal t in
      literal |> Option.map (fun o -> Ast.LiteralExpression o)
    | _ -> None

  and parse_binary_operator t =
    match t.token.typ with
    | Token.LOGICAL_AND -> Some Ast.Operators.Binary.(make AND)
    | Token.LOGICAL_OR -> Some Ast.Operators.Binary.(make OR)
    | Token.EQUAL_EQUAL -> Some Ast.Operators.Binary.(make EQUAL)
    | Token.NOT_EQUAL -> Some Ast.Operators.Binary.(make NOT_EQUAL)
    | Token.GREATER -> Some Ast.Operators.Binary.(make GREATER)
    | Token.GREATER_EQUAL -> Some Ast.Operators.Binary.(make GREATER_EQUAL)
    | Token.LESS -> Some Ast.Operators.Binary.(make LESS)
    | Token.LESS_EQUAL -> Some Ast.Operators.Binary.(make LESS_EQUAL)
    | Token.PLUSPLUS -> Some Ast.Operators.Binary.(make CONCAT)
    | Token.PLUS -> Some Ast.Operators.Binary.(make PLUS)
    | Token.MINUS -> Some Ast.Operators.Binary.(make MINUS)
    | Token.STAR -> Some Ast.Operators.Binary.(make TIMES)
    | Token.SLASH -> Some Ast.Operators.Binary.(make DIV)
    | Token.STAR_STAR -> Some Ast.Operators.Binary.(make POW)
    | Token.PERCENT -> Some Ast.Operators.Binary.(make MODULO)
    | Token.DOT -> Some Ast.Operators.Binary.(make DOT_ACCESS)
    | Token.ARROW_LEFT -> Some Ast.Operators.Binary.(make ARRAY_ADD)
    | Token.ATAT -> Some Ast.Operators.Binary.(make MERGE)
    | Token.LEFT_BRACK -> Some Ast.Operators.Binary.(make BRACKET_ACCESS)
    | _ -> None

  and parse_unary_operator t =
    match t.token.typ with
    | Token.NOT -> Some Ast.Operators.Unary.(make NOT)
    | Token.UNARY_MINUS -> Some Ast.Operators.Unary.(make MINUS)
    | _ -> None

  and parse_expression ?(prio = -999) t =
    let rec loop ~prio ~left t =
      match parse_binary_operator t with
      | None -> left
      | Some { precedence; _ } when precedence < prio -> left
      | Some { typ = operator; precedence; assoc; closing_token } ->
        next t;
        let new_prio =
          match assoc with
          | Left -> precedence + 1
          | Right -> precedence
        in
        (match parse_expression ~prio:new_prio t with
        | None ->
          failwith
            ("Expected expression on right hand side of "
            ^ (operator |> Ast.Operators.Binary.to_string))
        | Some right ->
          let left = Ast.BinaryExpression { left; operator; right } in
          let expect_close token = expect token t in
          let () = Option.iter expect_close closing_token in
          loop ~left ~prio t)
    in
    let* left = parse_expression_part t in
    Some (loop ~prio ~left t)

  and parse_statement t =
    match t.token.typ with
    | Token.KEYWORD_BREAK ->
      next t;
      Some Ast.BreakStmt
    | Token.KEYWORD_CONTINUE ->
      next t;
      Some Ast.ContinueStmt
    | Token.KEYWORD_LET ->
      next t;
      let identifier = Helpers.expect_identifier t in
      let nullable = t |> optional Token.QUESTIONMARK in
      t |> expect Token.EQUAL;
      let expression = parse_expression t in
      expect Token.SEMICOLON t;
      (match expression with
      | Some right -> Some (Ast.DeclarationStmt { nullable; left = Id identifier; right })
      | None -> assert false)
      (* TODO: Exception *)
    | _ ->
      (match parse_expression t with
      | Some expr -> Some (Ast.ExpressionStmt expr)
      | None -> None)

  and parse_declaration t =
    match t.token.typ with
    | ( Token.KEYWORD_SITE
      | Token.KEYWORD_PAGE
      | Token.KEYWORD_COMPONENT
      | Token.KEYWORD_STORE ) as typ ->
      let start_pos = t.token.start_pos in
      next t;
      let identifier = Helpers.expect_identifier ~typ:`Upper t in
      let identifier = Ast.Id identifier in
      let attributes =
        if optional Token.LEFT_PAREN t
        then (
          let attributes =
            Helpers.separated_list ~sep:Token.COMMA ~fn:parse_attribute t
          in
          t |> expect Token.RIGHT_PAREN;
          Some attributes)
        else None
      in
      let body = Helpers.non_empty_list ~fn:parse_statement t in
      (match typ with
      | Token.KEYWORD_SITE ->
        Some (Ast.SiteDeclaration { location = start_pos; identifier; attributes; body })
      | Token.KEYWORD_PAGE ->
        Some (Ast.PageDeclaration { location = start_pos; identifier; attributes; body })
      | Token.KEYWORD_COMPONENT ->
        Some
          (Ast.ComponentDeclaration { location = start_pos; identifier; attributes; body })
      | Token.KEYWORD_STORE ->
        Some (Ast.StoreDeclaration { location = start_pos; identifier; attributes; body })
      | _ -> assert false)
    | Token.END_OF_INPUT -> None
    | _ -> assert false
  ;;
end

let scan t =
  let declarations = t |> Helpers.list ~fn:Rules.parse_declaration in
  declarations
;;

let parse_file filename =
  let file_contents chan = really_input_string chan (in_channel_length chan) in
  let chan = open_in filename in
  let src = chan |> file_contents in
  close_in chan;
  let parser = make ~filename src in
  scan parser
;;
