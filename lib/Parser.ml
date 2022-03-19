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
  else
    Diagnostics.report
      ~start_pos:t.token.start_pos
      ~end_pos:t.token.end_pos
      (Diagnostics.Message
         (Printf.sprintf
            "Expected: `%s`, got `%s`"
            (Token.to_string token)
            (Token.to_string t.token.typ)))
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
        if has_sep || acc = []
        then loop (r :: acc)
        else
          Diagnostics.report
            ~start_pos:t.token.start_pos
            ~end_pos:t.token.end_pos
            (Diagnostics.Message
               (Printf.sprintf
                  "Expected list to be separated by `%s`"
                  (Token.to_string sep)))
      | None -> List.rev acc
    in
    loop []
  ;;

  let non_empty_separated_list ~sep ~fn t =
    let res = separated_list ~sep ~fn t in
    if res = []
    then
      Diagnostics.report
        ~start_pos:t.token.start_pos
        ~end_pos:t.token.end_pos
        (Diagnostics.Message (Printf.sprintf "Expected list to not be empty"))
    else res
  ;;

  let list ~fn t =
    let rec loop acc =
      match fn t with
      | Some r -> loop (r :: acc)
      | None -> List.rev acc
    in
    loop []
  ;;

  let non_empty_list ~fn t =
    let res = list ~fn t in
    if res = []
    then
      Diagnostics.report
        ~start_pos:t.token.start_pos
        ~end_pos:t.token.end_pos
        (Diagnostics.Message (Printf.sprintf "Expected list to not be empty"))
    else res
  ;;
end

module Rules = struct
  let rec parse_string_template t =
    match t.token.typ with
    | Token.STRING s ->
      next t;
      Some (Ast.StringText s)
    | Token.LEFT_PIPE_BRACE ->
      next t;
      let* expression = parse_expression t in
      t |> expect Token.RIGHT_PIPE_BRACE;
      Some (Ast.StringInterpolation expression)
    | _ -> None

  and parse_tag name t =
    next t;
    let attributes =
      if t |> optional Token.LEFT_PAREN
      then (
        let res =
          t
          |> Helpers.separated_list ~fn:parse_attribute ~sep:Token.COMMA
          |> List.to_seq
          |> Ast.StringMap.of_seq
        in
        t |> expect Token.RIGHT_PAREN;
        res)
      else Ast.StringMap.empty
    in
    let body =
      if t |> optional Token.DOUBLE_COLON
      then (
        let bind = t |> Helpers.expect_identifier ~typ:`Lower in
        t |> expect Token.ARROW;
        let body =
          match parse_expression t with
          | None ->
            Diagnostics.report
              ~start_pos:t.token.start_pos
              ~end_pos:t.token.end_pos
              (Diagnostics.Message
                 (Printf.sprintf "Expected expression as transformer of tag"))
          | Some expr -> expr
        in
        Some (Ast.Lowercase_Id bind, body))
      else None
    in
    match name with
    | "String" -> Some (Ast.TagString (attributes, body))
    | "Int" -> Some (Ast.TagInt (attributes, body))
    | "Float" -> Some (Ast.TagFloat (attributes, body))
    | "Boolean" -> Some (Ast.TagBoolean (attributes, body))
    | "Array" -> Some (Ast.TagArray (attributes, body))
    | "Record" -> Some (Ast.TagRecord (attributes, body))
    | "Slot" -> Some (Ast.TagSlot (attributes, body))
    | s ->
      Diagnostics.report
        ~start_pos:t.token.start_pos
        ~end_pos:t.token.end_pos
        (Diagnostics.Message (Printf.sprintf "Unknown tag with name `%s`." s))

  and parse_fn_param t =
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      Some key
    | _ -> None

  and parse_attribute ?(sep = Token.COLON) t =
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      expect sep t;
      let value = parse_expression t in
      value |> Option.map (fun value -> key, value)
    | _ -> None

  and parse_record_field t =
    match t.token.typ with
    | Token.IDENT_LOWER key ->
      next t;
      let nullable = optional Token.QUESTIONMARK t in
      expect Token.COLON t;
      let value = parse_expression t in
      value |> Option.map (fun value -> key, (nullable, value))
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
          (* TODO: This is `<div>{}</div>` ... Should this be an error? Probably yes... *)
          ExpressionTemplateNode (Ast.BlockExpression [])
      in
      t |> expect Token.RIGHT_BRACE;
      Some expression
    | Token.HTML_OPEN_TAG tag ->
      next t;
      let attributes =
        t
        |> Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL)
        |> List.to_seq
        |> Ast.StringMap.of_seq
      in
      let self_closing = t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING in
      let children =
        if self_closing
        then []
        else (
          t |> expect Token.HTML_OR_COMPONENT_TAG_END;
          let children = t |> Helpers.list ~fn:parse_template_node in
          t |> expect (Token.HTML_CLOSE_TAG tag);
          children)
      in
      Some (Ast.HtmlTemplateNode { tag; attributes; children; self_closing })
    | Token.COMPONENT_OPEN_TAG identifier ->
      next t;
      let attributes =
        t
        |> Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL)
        |> List.to_seq
        |> Ast.StringMap.of_seq
      in
      let self_closing = t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING in
      let children =
        if self_closing
        then []
        else (
          t |> expect Token.HTML_OR_COMPONENT_TAG_END;
          let children = t |> Helpers.list ~fn:parse_template_node in
          t |> expect (Token.COMPONENT_CLOSE_TAG identifier);
          children)
      in
      Some
        (Ast.ComponentTemplateNode
           { identifier = Uppercase_Id identifier; attributes; children })
    | _ -> None

  and parse_expression_part t =
    match t.token.typ with
    | Token.KEYWORD_BREAK ->
      next t;
      expect Token.KEYWORD_IF t;
      expect Token.LEFT_PAREN t;
      let* condition = parse_expression t in
      expect Token.RIGHT_PAREN t;
      expect Token.SEMICOLON t;
      Some (Ast.BreakExpression condition)
    | Token.KEYWORD_CONTINUE ->
      next t;
      expect Token.KEYWORD_IF t;
      expect Token.LEFT_PAREN t;
      let* condition = parse_expression t in
      expect Token.RIGHT_PAREN t;
      expect Token.SEMICOLON t;
      Some (Ast.ContinueExpression condition)
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
        | Token.IDENT_LOWER _ ->
          let token = peek t in
          token = Token.COLON || token = Token.QUESTIONMARK
        | _ -> false
      in
      if is_record
      then (
        let attrs =
          t
          |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_record_field
          |> List.to_seq
          |> Ast.StringMap.of_seq
        in
        t |> expect Token.RIGHT_BRACE;
        Some Ast.(Record attrs))
      else (
        let expressions = t |> Helpers.list ~fn:parse_expression in
        t |> expect Token.RIGHT_BRACE;
        Some (Ast.BlockExpression expressions))
    (* PARSING LET EXPRESSION *)
    | Token.KEYWORD_LET ->
      next t;
      let is_mutable = t |> optional Token.KEYWORD_MUTABLE in
      let identifier = Helpers.expect_identifier ~typ:`Lower t in
      let nullable = t |> optional Token.QUESTIONMARK in
      t |> expect Token.EQUAL;
      let expression = parse_expression t in
      expect Token.SEMICOLON t;
      (match expression with
      | Some expression when nullable ->
        Some (Ast.OptionalLetExpression (is_mutable, Lowercase_Id identifier, expression))
      | Some expression ->
        Some (Ast.LetExpression (is_mutable, Lowercase_Id identifier, expression))
      | None ->
        Diagnostics.report
          ~start_pos:t.token.start_pos
          ~end_pos:t.token.end_pos
          (Diagnostics.Message
             (Printf.sprintf "Expected expression as right hand side of let declaration")))
    (* PARSING FOR IN EXPRESSION *)
    | Token.KEYWORD_FOR ->
      next t;
      expect Token.LEFT_PAREN t;
      let index_or_iterator = Helpers.expect_identifier ~typ:`Lower t in
      let index, iterator =
        if optional Token.COMMA t
        then (
          let identifier = Helpers.expect_identifier ~typ:`Lower t in
          Some (Ast.Lowercase_Id index_or_iterator), identifier)
        else None, index_or_iterator
      in
      let iterator = Ast.Lowercase_Id iterator in
      expect Token.KEYWORD_IN t;
      let reverse = optional Token.KEYWORD_REVERSE t in
      let* expr1 = parse_expression t in
      expect Token.RIGHT_PAREN t;
      let body = Helpers.list ~fn:parse_expression t in
      (match body with
      | [] ->
        Diagnostics.report
          ~start_pos:t.token.start_pos
          ~end_pos:t.token.end_pos
          (Diagnostics.Message (Printf.sprintf "Expected expression as body of for loop"))
      | body ->
        Some (Ast.ForInExpression { index; iterator; reverse; iterable = expr1; body }))
    (* PARSING FN EXPRESSION *)
    | Token.KEYWORD_FN ->
      next t;
      expect Token.LEFT_PAREN t;
      let parameters = t |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_fn_param in
      expect Token.RIGHT_PAREN t;
      expect Token.ARROW t;
      let body = t |> Helpers.list ~fn:parse_expression in
      Some (Ast.Function (parameters, body))
    (* PARSING IF EXPRESSION *)
    | Token.KEYWORD_IF ->
      next t;
      expect Token.LEFT_PAREN t;
      let* condition = t |> parse_expression in
      expect Token.RIGHT_PAREN t;
      let consequent = t |> Helpers.list ~fn:parse_expression in
      let alternate =
        if optional Token.KEYWORD_ELSE t
        then Some (t |> Helpers.list ~fn:parse_expression)
        else None
      in
      Some (Ast.ConditionalExpression { condition; consequent; alternate })
    (* PARSING TAG EXPRESSION *)
    | Token.TAG name ->
      let* tag = parse_tag name t in
      Some (Ast.TagExpression tag)
    (* PARSING TEMPLATE EXPRESSION *)
    | Token.HTML_OPEN_TAG _ | Token.COMPONENT_OPEN_TAG _ ->
      let template_nodes = t |> Helpers.list ~fn:parse_template_node in
      Some (Ast.TemplateExpression template_nodes)
    (* PARSING TEMPLATE EXPRESSION *)
    | Token.HTML_OPEN_FRAGMENT ->
      next t;
      let template_nodes = t |> Helpers.list ~fn:parse_template_node in
      t |> expect Token.HTML_CLOSE_FRAGMENT;
      Some (Ast.TemplateExpression template_nodes)
    (* PARSING UNARY NOT EXPRESSION *)
    | Token.NOT ->
      next t;
      let operator = Ast.Operators.Unary.make NOT in
      let* argument = parse_expression ~prio:operator.precedence t in
      Some (Ast.UnaryExpression (Ast.Operators.Unary.NOT, argument))
    (* PARSING UNARY MINUS EXPRESSION *)
    | Token.UNARY_MINUS ->
      next t;
      let operator = Ast.Operators.Unary.make MINUS in
      let* argument = parse_expression ~prio:operator.precedence t in
      Some (Ast.UnaryExpression (Ast.Operators.Unary.MINUS, argument))
    (* PARSING IDENTIFIER OR MUTATION EXPRESSION *)
    | Token.IDENT_LOWER identifier ->
      next t;
      let is_mutation = optional Token.COLON_EQUAL t in
      if is_mutation
      then (
        let expression =
          match parse_expression t with
          | Some expression -> expression
          | None ->
            Diagnostics.report
              ~start_pos:t.token.start_pos
              ~end_pos:t.token.end_pos
              (Diagnostics.Message
                 (Printf.sprintf
                    "Expected expression as right hand side of mutation expression"))
        in
        expect Token.SEMICOLON t;
        Some (Ast.MutationExpression (Lowercase_Id identifier, expression)))
      else Some (Ast.LowercaseIdentifierExpression (Lowercase_Id identifier))
    | Token.IDENT_UPPER identifier ->
      next t;
      Some (Ast.UppercaseIdentifierExpression (Uppercase_Id identifier))
    (* PARSING VALUE EXPRESSION *)
    | Token.DOUBLE_QUOTE ->
      next t;
      let s = t |> Helpers.list ~fn:parse_string_template in
      t |> expect Token.DOUBLE_QUOTE;
      Some Ast.(String s)
    | Token.INT i ->
      next t;
      Some Ast.(Int i)
    | Token.FLOAT f ->
      next t;
      Some Ast.(Float f)
    | Token.KEYWORD_TRUE ->
      next t;
      Some Ast.(Bool true)
    | Token.KEYWORD_FALSE ->
      next t;
      Some Ast.(Bool false)
    | Token.LEFT_BRACK ->
      next t;
      let expressions =
        Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression t |> Iter.of_list
      in
      expect Token.RIGHT_BRACK t;
      Some Ast.(Array expressions)
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
    | Token.LEFT_PAREN -> Some Ast.Operators.Binary.(make FUNCTION_CALL)
    | Token.DOTDOT -> Some Ast.Operators.Binary.(make RANGE)
    | Token.DOTDOTDOT -> Some Ast.Operators.Binary.(make INCLUSIVE_RANGE)
    | Token.PIPE -> Some Ast.Operators.Binary.(make PIPE)
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
      | Some { typ = Ast.Operators.Binary.FUNCTION_CALL; closing_token; _ } ->
        next t;
        let arguments =
          t |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression
        in
        let left = Ast.FunctionCall (left, arguments) in
        let expect_close token = expect token t in
        let () = Option.iter expect_close closing_token in
        loop ~left ~prio t
      | Some { typ = operator; precedence; assoc; closing_token } ->
        next t;
        let new_prio =
          match assoc with
          | Left -> precedence + 1
          | Right -> precedence
        in
        (match parse_expression ~prio:new_prio t with
        | None ->
          Diagnostics.report
            ~start_pos:t.token.start_pos
            ~end_pos:t.token.end_pos
            (Diagnostics.Message
               (Printf.sprintf
                  "Expected expression on right hand side of `%s`"
                  (Ast.Operators.Binary.to_string operator)))
        | Some right ->
          let left = Ast.BinaryExpression (left, operator, right) in
          let expect_close token = expect token t in
          let () = Option.iter expect_close closing_token in
          loop ~left ~prio t)
    in
    let* left = parse_expression_part t in
    Some (loop ~prio ~left t)

  and parse_declaration t =
    match t.token.typ with
    | ( Token.KEYWORD_SITE
      | Token.KEYWORD_PAGE
      | Token.KEYWORD_COMPONENT
      | Token.KEYWORD_STORE ) as typ ->
      next t;
      let identifier = Helpers.expect_identifier ~typ:`Upper t in
      let attributes =
        if optional Token.LEFT_PAREN t
        then (
          let attributes =
            Helpers.separated_list ~sep:Token.COMMA ~fn:parse_attribute t
            |> List.to_seq
            |> Ast.StringMap.of_seq
          in
          t |> expect Token.RIGHT_PAREN;
          Some attributes)
        else None
      in
      t |> expect Token.LEFT_BRACE;
      let body = t |> Helpers.list ~fn:parse_expression in
      t |> expect Token.RIGHT_BRACE;
      (match body with
      | [] ->
        Diagnostics.report
          ~start_pos:t.token.start_pos
          ~end_pos:t.token.end_pos
          (Diagnostics.Message "Expected declaration to have a body")
      | body ->
        (match typ with
        | Token.KEYWORD_SITE -> Some (identifier, Ast.SiteDeclaration (attributes, body))
        | Token.KEYWORD_PAGE -> Some (identifier, Ast.PageDeclaration (attributes, body))
        | Token.KEYWORD_COMPONENT ->
          Some (identifier, Ast.ComponentDeclaration (attributes, body))
        | Token.KEYWORD_STORE -> Some (identifier, Ast.StoreDeclaration (attributes, body))
        | _ -> assert false))
    | Token.END_OF_INPUT -> None
    | _ -> assert false
  ;;
end

let scan t =
  t |> Helpers.list ~fn:Rules.parse_declaration |> List.to_seq |> Ast.StringMap.of_seq
;;

let parse_file filename =
  let file_contents chan = really_input_string chan (in_channel_length chan) in
  let chan = open_in filename in
  let src = chan |> file_contents in
  close_in chan;
  let parser = make ~filename src in
  scan parser
;;
