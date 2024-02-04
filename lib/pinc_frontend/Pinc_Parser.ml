module Ast = Pinc_Ast
module Operators = Ast.Operators
module Diagnostics = Pinc_Diagnostics
module Location = Diagnostics.Location
module Token = Pinc_Token
module Lexer = Pinc_Lexer

type t = {
  lexer : Lexer.t;
  mutable prev_token : Token.t;
  mutable token : Token.t;
  next : Token.t Queue.t;
}

let ( let* ) = Option.bind

let get_next_token t =
  match Queue.take_opt t.next with
  | None -> Lexer.scan t.lexer
  | Some token -> token
;;

let next t =
  let token = get_next_token t in
  t.prev_token <- t.token;
  t.token <- token
;;

let peek t =
  let token = get_next_token t in
  Queue.add token t.next;
  token.typ
;;

let optional token t =
  let test = t.token.typ = token in
  if test then
    next t;
  test
;;

let expect token t =
  let test = t.token.typ = token in
  if test then
    next t
  else
    Diagnostics.error
      t.token.location
      (Printf.sprintf
         "Expected: `%s`, got `%s`"
         (Token.to_string token)
         (Token.to_string t.token.typ))
;;

let make ~filename src =
  let lexer = Lexer.make ~filename src in
  let initial_pos = Location.Position.make ~filename ~line:0 ~column:0 in
  let loc = Location.make ~s:initial_pos () in
  let initial_token = Token.make ~loc Token.END_OF_INPUT in
  let t =
    { lexer; prev_token = initial_token; token = initial_token; next = Queue.create () }
  in
  next t;
  t
;;

module Helpers = struct
  let expect_identifier ?error_message ?(typ = `All) t =
    let location =
      Location.make ~s:t.token.location.loc_start ~e:t.token.location.loc_end ()
    in
    match t.token.typ with
    | Token.IDENT_UPPER i when typ = `Upper || typ = `All ->
        next t;
        (i, location)
    | Token.IDENT_LOWER i when typ = `Lower || typ = `All ->
        next t;
        (i, location)
    | token when typ = `Lower ->
        Diagnostics.error
          location
          (match (error_message, token) with
          | Some message, _ -> message
          | None, Token.IDENT_UPPER i ->
              "Expected to see a lowercase identifier at this point. Did you mean "
              ^ String.lowercase_ascii i
              ^ " instead of "
              ^ i
              ^ "?"
          | None, t ->
              "Expected to see a lowercase identifier at this point. Instead saw "
              ^ Token.to_string t)
    | token when typ = `Upper ->
        Diagnostics.error
          location
          (match (error_message, token) with
          | Some message, _ -> message
          | None, Token.IDENT_LOWER i ->
              "Expected to see an uppercase identifier at this point. Did you mean "
              ^ String.capitalize_ascii i
              ^ " instead of "
              ^ i
              ^ "?"
          | None, _ -> "Expected to see an uppercase identifier at this point.")
    | token ->
        Diagnostics.error
          location
          (match (error_message, token) with
          | Some message, _ -> message
          | None, t when Token.is_keyword t ->
              "\"" ^ Token.to_string t ^ "\" is a keyword. Please choose another name."
          | None, _ -> "Expected to see an identifier at this point.")
  ;;

  let separated_list ~sep ~fn t =
    let rec loop acc =
      let has_sep = optional sep t in
      match fn t with
      | Some r ->
          if has_sep || acc = [] then
            loop (r :: acc)
          else
            Diagnostics.error
              t.token.location
              ("Expected list to be separated by `" ^ Token.to_string sep ^ "`")
      | None -> List.rev acc
    in
    loop []
  ;;

  let list ~fn t =
    let rec loop acc =
      match fn t with
      | Some r -> loop (r :: acc)
      | None -> List.rev acc
    in
    loop []
  ;;
end

module Rules = struct
  let rec parse_string_template t =
    let string_template_start = t.token.location.loc_start in
    let* string_template_desc =
      match t.token.typ with
      | Token.STRING s ->
          next t;
          Some (Ast.StringText s)
      | Token.OPEN_TEMPLATE_LITERAL ->
          next t;
          let identifier =
            t
            |> Helpers.expect_identifier
                 ~typ:`Lower
                 ~error_message:
                   "Only lowercase identifiers are allowed as string interpolation \
                    placeholders.\n\
                    If you want to use more complex constructs, you can always assign \
                    them to a let binding."
          in
          t |> expect Token.RIGHT_PAREN;
          Some (Ast.StringInterpolation (Lowercase_Id identifier))
      | _ -> None
    in
    let string_template_end = t.token.location.loc_end in
    let string_template_loc =
      Location.make ~s:string_template_start ~e:string_template_end ()
    in
    Ast.{ string_template_desc; string_template_loc } |> Option.some

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
        value |> Option.map (fun value -> (key, value))
    | _ -> None

  and parse_record_field t =
    match t.token.typ with
    | Token.IDENT_LOWER key ->
        next t;
        let nullable = optional Token.QUESTIONMARK t in
        expect Token.COLON t;
        let value = parse_expression t in
        value |> Option.map (fun value -> (key, (nullable, value)))
    | _ -> None

  and parse_block t =
    let expr_start = t.token.location.loc_start in
    let* expression_desc =
      match t.token.typ with
      | Token.LEFT_BRACE ->
          next t;
          let statements = t |> Helpers.list ~fn:parse_statement in
          t |> expect Token.RIGHT_BRACE;
          Ast.BlockExpression statements |> Option.some
      | _ ->
          let* statement = parse_statement t in
          Ast.BlockExpression [ statement ] |> Option.some
    in
    let expr_end = t.token.location.loc_end in
    let expression_loc = Location.make ~s:expr_start ~e:expr_end () in
    Ast.{ expression_desc; expression_loc } |> Option.some

  and parse_tag ~name t =
    let start_token = t.token in
    let attributes =
      if t |> optional Token.LEFT_PAREN then (
        let res =
          t
          |> Helpers.separated_list ~fn:parse_attribute ~sep:Token.COMMA
          |> List.to_seq
          |> StringMap.of_seq
        in
        t |> expect Token.RIGHT_PAREN;
        res)
      else
        StringMap.empty
    in
    let transformer =
      let start_token = t.token in
      if t |> optional Token.DOUBLE_COLON then (
        let open_paren = t |> optional Token.LEFT_PAREN in
        let bind = t |> Helpers.expect_identifier ~typ:`Lower in
        if open_paren then
          t |> expect Token.RIGHT_PAREN;
        t |> expect Token.ARROW;
        let body =
          match parse_expression t with
          | None ->
              Diagnostics.error
                (Location.make
                   ~s:start_token.location.loc_start
                   ~e:t.token.location.loc_end
                   ())
                "This tag transformer does not have a valid body."
          | Some expr -> expr
        in
        Some (Ast.Lowercase_Id bind, body))
      else
        None
    in
    let tag =
      match name with
      | "String" -> Ast.Tag_String
      | "Int" -> Ast.Tag_Int
      | "Float" -> Ast.Tag_Float
      | "Boolean" -> Ast.Tag_Boolean
      | "Array" -> Ast.Tag_Array
      | "Record" -> Ast.Tag_Record
      | "Slot" -> Ast.Tag_Slot
      | "Store" -> Ast.Tag_Store
      | "SetContext" -> Ast.Tag_SetContext
      | "GetContext" -> Ast.Tag_GetContext
      | "CreatePortal" -> Ast.Tag_CreatePortal
      | "Portal" -> Ast.Tag_Portal
      | other -> Ast.Tag_Custom other
    in
    let tag_loc =
      Location.make ~s:start_token.location.loc_start ~e:t.token.location.loc_end ()
    in
    let tag_desc = Ast.{ tag; attributes; transformer } in
    Ast.TagExpression Ast.{ tag_loc; tag_desc } |> Option.some

  and parse_template_node t =
    let node_start = t.token in
    let* template_node_desc =
      match t.token.typ with
      | Token.STRING s ->
          next t;
          Some (Ast.TextTemplateNode s)
      | Token.LEFT_BRACE ->
          let start_token = t.token in
          next t;
          let has_comment =
            match t.token.typ with
            | Token.COMMENT _ -> true
            | _ -> false
          in
          let expression =
            match parse_expression t with
            | Some e -> Ast.ExpressionTemplateNode e
            | None ->
                let location =
                  Location.make
                    ~s:start_token.location.loc_start
                    ~e:t.token.location.loc_end
                    ()
                in
                if not has_comment then
                  Diagnostics.warn
                    location
                    "Expected to see an expression between these braces. \n\
                     This is currently not doing anything, so you can safely remove it.\n\
                     If you wanted to have an empty record here, you need to write `{{}}`";
                ExpressionTemplateNode
                  { expression_loc = location; expression_desc = Ast.BlockExpression [] }
          in
          t |> expect Token.RIGHT_BRACE;
          Some expression
      | Token.HTML_OPEN_TAG html_tag_identifier ->
          next t;
          let html_tag_attributes =
            t
            |> Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL)
            |> List.to_seq
            |> StringMap.of_seq
          in
          let html_tag_self_closing =
            t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING
          in
          let html_tag_children =
            if html_tag_self_closing then
              []
            else (
              t |> expect Token.HTML_OR_COMPONENT_TAG_END;
              let children = t |> Helpers.list ~fn:parse_template_node in
              t |> expect (Token.HTML_CLOSE_TAG html_tag_identifier);
              children)
          in
          Some
            (Ast.HtmlTemplateNode
               {
                 html_tag_identifier;
                 html_tag_attributes;
                 html_tag_children;
                 html_tag_self_closing;
               })
      | Token.COMPONENT_OPEN_TAG identifier ->
          let component_tag_identifier =
            Ast.Uppercase_Id (identifier, t.token.location)
          in
          next t;
          let component_tag_attributes =
            t
            |> Helpers.list ~fn:(parse_attribute ~sep:Token.EQUAL)
            |> List.to_seq
            |> StringMap.of_seq
          in
          let component_tag_self_closing =
            t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING
          in
          let component_tag_children =
            if component_tag_self_closing then
              []
            else (
              t |> expect Token.HTML_OR_COMPONENT_TAG_END;
              let children = t |> Helpers.list ~fn:parse_template_node in
              t |> expect (Token.COMPONENT_CLOSE_TAG identifier);
              children)
          in
          Some
            (Ast.ComponentTemplateNode
               {
                 component_tag_identifier;
                 component_tag_attributes;
                 component_tag_children;
               })
      | _ -> None
    in
    let node_end = t.prev_token in
    let template_node_loc =
      Location.make ~s:node_start.location.loc_start ~e:node_end.location.loc_end ()
    in
    Ast.{ template_node_desc; template_node_loc } |> Option.some

  and parse_statement t =
    let statement_start = t.token in
    let* statement_desc =
      match t.token.typ with
      (* PARSING BREAK STATEMENT *)
      | Token.KEYWORD_BREAK ->
          next t;
          let num_loops =
            match t.token.typ with
            | Token.INT i ->
                next t;
                i
            | _ -> 1
          in
          let _ = optional Token.SEMICOLON t in
          Some (Ast.BreakStatement num_loops)
      (* PARSING CONTINUE STATEMENT *)
      | Token.KEYWORD_CONTINUE ->
          next t;
          let num_loops =
            match t.token.typ with
            | Token.INT i ->
                next t;
                i
            | _ -> 1
          in
          let _ = optional Token.SEMICOLON t in
          Some (Ast.ContinueStatement num_loops)
      (* PARSING LET STATEMENT *)
      | Token.KEYWORD_LET -> (
          let start_token = t.token in
          next t;
          let is_mutable = t |> optional Token.KEYWORD_MUTABLE in
          let identifier = Helpers.expect_identifier ~typ:`Lower t in
          let is_nullable = t |> optional Token.QUESTIONMARK in
          t |> expect Token.EQUAL;
          let end_token = t.token in
          let expression = parse_expression t in
          let _ = optional Token.SEMICOLON t in
          match (is_mutable, is_nullable, expression) with
          | false, true, Some expression ->
              Some (Ast.OptionalLetStatement (Lowercase_Id identifier, expression))
          | true, true, Some expression ->
              Some (Ast.OptionalMutableLetStatement (Lowercase_Id identifier, expression))
          | false, false, Some expression ->
              Some (Ast.LetStatement (Lowercase_Id identifier, expression))
          | true, false, Some expression ->
              Some (Ast.MutableLetStatement (Lowercase_Id identifier, expression))
          | _, _, None ->
              Diagnostics.error
                (Location.make
                   ~s:start_token.location.loc_start
                   ~e:end_token.location.loc_end
                   ())
                "Expected expression as right hand side of let declaration")
      (* PARSING MUTATION STATEMENT *)
      | Token.IDENT_LOWER identifier when peek t = Token.COLON_EQUAL ->
          let start_token = t.token in
          let identifier_location = t.token.location in
          next t;
          next t;
          let end_token = t.token in
          let expression =
            match parse_expression t with
            | Some expression -> expression
            | None ->
                Diagnostics.error
                  (Location.make
                     ~s:start_token.location.loc_start
                     ~e:end_token.location.loc_end
                     ())
                  "Expected expression as right hand side of mutation statement"
          in
          let _ = optional Token.SEMICOLON t in
          Some
            (Ast.MutationStatement
               (Lowercase_Id (identifier, identifier_location), expression))
      (* PARSING USE STATEMENT *)
      | Token.KEYWORD_USE ->
          next t;
          let is_assignment = peek t = Token.EQUAL in
          if is_assignment then (
            let identifier = Helpers.expect_identifier ~typ:`Upper t in
            t |> expect Token.EQUAL;
            let expression =
              match parse_expression t with
              | Some expression -> expression
              | None ->
                  Diagnostics.error
                    t.token.location
                    "Expected expression as right hand side of use statement"
            in
            let _ = optional Token.SEMICOLON t in
            Some (Ast.UseStatement (Some (Uppercase_Id identifier), expression)))
          else (
            let expression =
              match parse_expression t with
              | Some expression -> expression
              | None ->
                  Diagnostics.error
                    t.token.location
                    "Expected to see a library next to the use keyword."
            in
            let _ = optional Token.SEMICOLON t in
            Some (Ast.UseStatement (None, expression)))
      | _ ->
          let* expr = parse_expression t in
          Some (Ast.ExpressionStatement expr)
    in
    let statement_end = t.token in
    let statement_loc =
      Location.make
        ~s:statement_start.location.loc_start
        ~e:statement_end.location.loc_end
        ()
    in
    Ast.{ statement_desc; statement_loc } |> Option.some

  and parse_unary_expression t =
    let* operator =
      match t.token.typ with
      | Token.NOT -> Some Operators.Unary.NOT
      | Token.MINUS -> Some Operators.Unary.MINUS
      | _ -> None
    in
    next t;
    let* argument = parse_expression ~prio:(Operators.Unary.get_precedence operator) t in
    Ast.UnaryExpression (operator, argument) |> Option.some

  and parse_expression_part t =
    let expr_start = t.token.location.loc_start in
    let* expression_desc =
      match t.token.typ with
      (* PARSING COMMENT EXPRESSION *)
      | Token.COMMENT s ->
          next t;
          Some (Ast.Comment s)
      (* PARSING PARENTHESIZED EXPRESSION *)
      | Token.LEFT_PAREN ->
          next t;
          let expr = parse_expression t in
          t |> expect Token.RIGHT_PAREN;
          expr |> Option.map (fun expr -> expr.Ast.expression_desc)
      (* PARSING RECORD or BLOCK EXPRESSION *)
      | Token.LEFT_BRACE ->
          next t;
          let is_record =
            match t.token.typ with
            | Token.IDENT_LOWER _ ->
                let token = peek t in
                token = Token.COLON || token = Token.QUESTIONMARK
            | Token.RIGHT_BRACE -> true
            | _ -> false
          in
          if is_record then (
            let attrs =
              t
              |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_record_field
              |> StringMap.of_list
            in
            t |> expect Token.RIGHT_BRACE;
            Ast.Record attrs |> Option.some)
          else (
            let statements = t |> Helpers.list ~fn:parse_statement in
            t |> expect Token.RIGHT_BRACE;
            Ast.BlockExpression statements |> Option.some)
      (* PARSING FOR IN EXPRESSION *)
      | Token.KEYWORD_FOR -> (
          next t;
          let _ = optional Token.LEFT_PAREN t in
          let index_or_iterator = Helpers.expect_identifier ~typ:`Lower t in
          let index, iterator =
            if optional Token.COMMA t then (
              let identifier = Helpers.expect_identifier ~typ:`Lower t in
              (Some (Ast.Lowercase_Id index_or_iterator), identifier))
            else
              (None, index_or_iterator)
          in
          let iterator = Ast.Lowercase_Id iterator in
          expect Token.KEYWORD_IN t;
          let reverse = optional Token.KEYWORD_REVERSE t in
          let* expr1 = parse_expression t in
          let _ = optional Token.RIGHT_PAREN t in
          let body = parse_block t in
          match body with
          | None ->
              Diagnostics.error t.token.location "Expected expression as body of for loop"
          | Some body ->
              Ast.ForInExpression { index; iterator; reverse; iterable = expr1; body }
              |> Option.some)
      (* PARSING FN EXPRESSION *)
      | Token.KEYWORD_FN ->
          let fn_loc = t.token.location in
          next t;
          let open_paren = t |> optional Token.LEFT_PAREN in
          let parameters =
            if open_paren then (
              let params =
                t |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_fn_param
              in
              t |> expect Token.RIGHT_PAREN;
              params)
            else (
              match t |> parse_fn_param with
              | Some p -> [ p ]
              | None ->
                  Diagnostics.error
                    fn_loc
                    "Expected this function to have exactly one parameter")
          in
          expect Token.ARROW t;
          let* body = t |> parse_block in
          Ast.Function { parameters; body } |> Option.some
      (* PARSING IF EXPRESSION *)
      | Token.KEYWORD_IF ->
          next t;
          let _ = optional Token.LEFT_PAREN t in
          let* condition = t |> parse_expression in
          let _ = optional Token.RIGHT_PAREN t in
          let* consequent = t |> parse_statement in
          let alternate =
            if optional Token.KEYWORD_ELSE t then
              t |> parse_statement
            else
              None
          in
          Ast.ConditionalExpression { condition; consequent; alternate } |> Option.some
      (* PARSING TAG EXPRESSION *)
      | Token.TAG name ->
          next t;
          parse_tag ~name t
      (* PARSING TEMPLATE EXPRESSION *)
      | Token.HTML_OPEN_TAG _ | Token.COMPONENT_OPEN_TAG _ ->
          let template_nodes = t |> Helpers.list ~fn:parse_template_node in
          Ast.TemplateExpression template_nodes |> Option.some
      (* PARSING TEMPLATE EXPRESSION *)
      | Token.HTML_OPEN_FRAGMENT ->
          next t;
          let template_nodes = t |> Helpers.list ~fn:parse_template_node in
          t |> expect Token.HTML_CLOSE_FRAGMENT;
          Ast.TemplateExpression template_nodes |> Option.some
      (* PARSING IDENTIFIER EXPRESSION *)
      | Token.IDENT_LOWER identifier ->
          next t;
          Ast.LowercaseIdentifierExpression identifier |> Option.some
      | Token.IDENT_UPPER identifier -> (
          next t;
          let rec get_path list =
            match t.token.typ with
            | Token.DOT -> (
                match peek t with
                | Token.IDENT_UPPER i ->
                    next t;
                    next t;
                    get_path (i :: list)
                | _ -> List.rev list)
            | _ -> List.rev list
          in
          match get_path [ identifier ] with
          | [ identifier ] -> Ast.UppercaseIdentifierExpression identifier |> Option.some
          | path -> Ast.UppercaseIdentifierPathExpression path |> Option.some)
      (* PARSING VALUE EXPRESSION *)
      | Token.DOUBLE_QUOTE ->
          next t;
          let s = t |> Helpers.list ~fn:parse_string_template in
          t |> expect Token.DOUBLE_QUOTE;
          Ast.(String s) |> Option.some
      | Token.INT i ->
          next t;
          Ast.(Int i) |> Option.some
      | Token.CHAR c ->
          next t;
          Ast.(Char c) |> Option.some
      | Token.FLOAT f ->
          next t;
          Ast.(Float f) |> Option.some
      | Token.KEYWORD_TRUE ->
          next t;
          Ast.(Bool true) |> Option.some
      | Token.KEYWORD_FALSE ->
          next t;
          Ast.(Bool false) |> Option.some
      | Token.LEFT_BRACK ->
          next t;
          let expressions =
            Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression t
            |> Array.of_list
          in
          expect Token.RIGHT_BRACK t;
          Ast.(Array expressions) |> Option.some
      | _ -> parse_unary_expression t
    in
    let expr_end = t.token.location.loc_end in
    let expression_loc = Location.make ~s:expr_start ~e:expr_end () in
    Ast.{ expression_desc; expression_loc } |> Option.some

  and parse_binary_operator t =
    match t.token.typ with
    | Token.LOGICAL_AND -> Some Operators.Binary.AND
    | Token.LOGICAL_OR -> Some Operators.Binary.OR
    | Token.EQUAL_EQUAL -> Some Operators.Binary.EQUAL
    | Token.NOT_EQUAL -> Some Operators.Binary.NOT_EQUAL
    | Token.GREATER -> Some Operators.Binary.GREATER
    | Token.GREATER_EQUAL -> Some Operators.Binary.GREATER_EQUAL
    | Token.LESS -> Some Operators.Binary.LESS
    | Token.LESS_EQUAL -> Some Operators.Binary.LESS_EQUAL
    | Token.PLUSPLUS -> Some Operators.Binary.CONCAT
    | Token.PLUS -> Some Operators.Binary.PLUS
    | Token.MINUS -> Some Operators.Binary.MINUS
    | Token.STAR -> Some Operators.Binary.TIMES
    | Token.SLASH -> Some Operators.Binary.DIV
    | Token.STAR_STAR -> Some Operators.Binary.POW
    | Token.PERCENT -> Some Operators.Binary.MODULO
    | Token.DOT -> Some Operators.Binary.DOT_ACCESS
    | Token.ARROW_LEFT -> Some Operators.Binary.ARRAY_ADD
    | Token.ATAT -> Some Operators.Binary.MERGE
    | Token.LEFT_BRACK -> Some Operators.Binary.BRACKET_ACCESS
    | Token.LEFT_PAREN -> Some Operators.Binary.FUNCTION_CALL
    | Token.DOTDOT -> Some Operators.Binary.RANGE
    | Token.DOTDOTDOT -> Some Operators.Binary.INCLUSIVE_RANGE
    | Token.PIPE -> Some Operators.Binary.PIPE
    | _ -> None

  and parse_expression ?(prio = -999) t =
    let rec loop ~prio ~left t =
      if optional Token.SEMICOLON t then
        left
      else (
        match parse_binary_operator t with
        | None -> left
        | Some operator ->
            let precedence = Operators.Binary.get_precedence operator in
            if precedence < prio then
              left
            else (
              let expression_start = t.token.location.loc_start in
              next t;
              let expression_desc =
                match operator with
                | Operators.Binary.FUNCTION_CALL ->
                    let arguments =
                      t |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression
                    in
                    Ast.FunctionCall { function_definition = left; arguments }
                | operator -> (
                    let new_prio =
                      match Operators.Binary.get_associativity operator with
                      | Assoc_Left -> precedence + 1
                      | Assoc_Right -> precedence
                    in
                    match parse_expression ~prio:new_prio t with
                    | None ->
                        Diagnostics.error
                          t.token.location
                          ("Expected expression on right hand side of `"
                          ^ Operators.Binary.to_string operator
                          ^ "`")
                    | Some right -> Ast.BinaryExpression (left, operator, right))
              in
              let expect_close token = expect token t in
              Operators.Binary.get_closing_token operator |> Option.iter expect_close;
              let expression_end = t.token.location.loc_end in
              let expression_loc =
                Location.make ~s:expression_start ~e:expression_end ()
              in
              let left = Ast.{ expression_desc; expression_loc } in
              loop ~left ~prio t))
    in
    let* left = parse_expression_part t in
    Some (loop ~prio ~left t)

  and parse_declaration t =
    let declaration_start = t.token.location.loc_start in
    let* identifier, declaration_type =
      match t.token.typ with
      | ( Token.KEYWORD_PAGE
        | Token.KEYWORD_COMPONENT
        | Token.KEYWORD_LIBRARY
        | Token.KEYWORD_STORE ) as typ -> (
          next t;
          let identifier = Helpers.expect_identifier ~typ:`Upper t |> fst in
          let declaration_attributes =
            if optional Token.LEFT_PAREN t then (
              let attributes =
                Helpers.separated_list ~sep:Token.COMMA ~fn:parse_attribute t
                |> List.to_seq
                |> StringMap.of_seq
              in
              t |> expect Token.RIGHT_PAREN;
              attributes)
            else
              StringMap.empty
          in
          let declaration_body = t |> parse_expression in
          match declaration_body with
          | None ->
              Diagnostics.error t.token.location "Expected declaration to have a body"
          | Some declaration_body -> (
              let declaration_desc = Ast.{ declaration_attributes; declaration_body } in
              match typ with
              | Token.KEYWORD_PAGE ->
                  Some (identifier, Ast.Declaration_Page declaration_desc)
              | Token.KEYWORD_COMPONENT ->
                  Some (identifier, Ast.Declaration_Component declaration_desc)
              | Token.KEYWORD_STORE ->
                  Some (identifier, Ast.Declaration_Store declaration_desc)
              | Token.KEYWORD_LIBRARY ->
                  Some (identifier, Ast.Declaration_Library declaration_desc)
              | _ -> assert false))
      | Token.END_OF_INPUT -> None
      | _ ->
          Diagnostics.error
            t.token.location
            "Expected to see a declaration (page, component, store or library)."
    in
    let declaration_end = t.token.location.loc_end in
    let declaration_loc = Location.make ~s:declaration_start ~e:declaration_end () in
    (identifier, Ast.{ declaration_loc; declaration_type }) |> Option.some
  ;;
end

let scan : t -> Ast.t =
 fun t -> t |> Helpers.list ~fn:Rules.parse_declaration |> List.to_seq |> StringMap.of_seq
;;

let parse ~filename source =
  let parser = make ~filename source in
  scan parser
;;
