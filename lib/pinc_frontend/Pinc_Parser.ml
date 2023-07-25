module Ast = Pinc_Ast
module Diagnostics = Pinc_Diagnostics
module Location = Diagnostics.Location
module Token = Pinc_Token
module Lexer = Pinc_Lexer

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
  next : Token.t Queue.t;
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
  let t = { lexer; token = initial_token; next = Queue.create () } in
  next t;
  t
;;

module Helpers = struct
  let expect_identifier ?(typ = `All) t =
    let start_pos = t.token.location.loc_start in
    match t.token.typ with
    | Token.IDENT_UPPER i when typ = `Upper || typ = `All ->
        next t;
        i
    | Token.IDENT_LOWER i when typ = `Lower || typ = `All ->
        next t;
        i
    | token when typ = `Lower ->
        Diagnostics.error
          (Location.make ~s:start_pos ~e:t.token.location.loc_end ())
          (match token with
          | Token.IDENT_UPPER i ->
              "Expected to see a lowercase identifier at this point. Did you mean "
              ^ String.lowercase_ascii i
              ^ " instead of "
              ^ i
              ^ "?"
          | t when Token.is_keyword t ->
              "`" ^ Token.to_string t ^ "` is a keyword. Please choose another name."
          | t ->
              "Expected to see a lowercase identifier at this point. Instead saw "
              ^ Token.to_string t)
    | token when typ = `Upper ->
        Diagnostics.error
          (Location.make ~s:start_pos ~e:t.token.location.loc_end ())
          (match token with
          | Token.IDENT_LOWER i ->
              "Expected to see an uppercase identifier at this point. Did you mean "
              ^ String.capitalize_ascii i
              ^ " instead of "
              ^ i
              ^ "?"
          | _ -> "Expected to see an uppercase identifier at this point.")
    | token ->
        Diagnostics.error
          (Location.make ~s:start_pos ~e:t.token.location.loc_end ())
          (match token with
          | t when Token.is_keyword t ->
              "\"" ^ Token.to_string t ^ "\" is a keyword. Please choose another name."
          | _ -> "Expected to see an identifier at this point.")
  ;;

  let separated_list ~sep ~fn t =
    let rec loop acc =
      let has_sep = optional sep t in
      let location = Location.make ~s:t.token.location.loc_start in
      match fn t with
      | Some r ->
          let location = location ~e:t.token.location.loc_end in
          if has_sep || acc = [] then
            loop ((r |> Location.add (location ())) :: acc)
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
      let location = Location.make ~s:t.token.location.loc_start in
      match fn t with
      | Some r ->
          let location = location ~e:t.token.location.loc_end in
          loop ((r |> Location.add (location ())) :: acc)
      | None -> List.rev acc
    in
    loop []
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

  and parse_tag ~name t =
    let start_token = t.token in
    let attributes =
      if t |> optional Token.LEFT_PAREN then (
        let start_token = t.token in
        let res =
          t
          |> Helpers.separated_list ~fn:parse_attribute ~sep:Token.COMMA
          |> List.to_seq
          |> Seq.map (fun attr ->
                 let location = attr |> Location.get in
                 let key, value = attr |> Location.get_data in
                 (key, value |> Location.add location))
          |> StringMap.of_seq
        in
        t |> expect Token.RIGHT_PAREN;
        let end_token = t.token in
        let location =
          Location.make ~s:start_token.location.loc_start ~e:end_token.location.loc_end ()
        in
        Some (res |> Location.add location))
      else
        None
    in
    let transformer =
      let start_token = t.token in
      if t |> optional Token.DOUBLE_COLON then (
        let bind = t |> Helpers.expect_identifier ~typ:`Lower in
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
        let location =
          Location.make ~s:start_token.location.loc_start ~e:t.token.location.loc_end ()
        in
        Some ((Ast.Lowercase_Id bind, body) |> Location.add location))
      else
        None
    in
    let tag =
      match name with
      | "String" -> `String
      | "Int" -> `Int
      | "Float" -> `Float
      | "Boolean" -> `Boolean
      | "Array" -> `Array
      | "Record" -> `Record
      | "Slot" -> `Slot
      | "SetContext" -> `SetContext
      | "GetContext" -> `GetContext
      | "CreatePortal" -> `CreatePortal
      | "Portal" -> `Portal
      | other -> `Custom other
    in
    let location =
      Location.make ~s:start_token.location.loc_start ~e:t.token.location.loc_end ()
    in
    Some
      (Ast.TagExpression (Ast.{ tag; attributes; transformer } |> Location.add location))

  and parse_template_node t =
    match t.token.typ with
    | Token.STRING s ->
        next t;
        Some (Ast.TextTemplateNode s)
    | Token.LEFT_BRACE ->
        let start_token = t.token in
        next t;
        let expression =
          match parse_expression t with
          | Some e -> Ast.ExpressionTemplateNode e
          | None ->
              Diagnostics.warn
                (Location.make
                   ~s:start_token.location.loc_start
                   ~e:t.token.location.loc_end
                   ())
                "Expected to see an expression between these braces. \n\
                 This is currently not doing anything, so you can safely remove it.";
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
          |> Seq.map (fun attr ->
                 let location = attr |> Location.get in
                 let key, value = attr |> Location.get_data in
                 (key, value |> Location.add location))
          |> StringMap.of_seq
        in
        let self_closing = t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING in
        let children =
          if self_closing then
            []
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
          |> Seq.map (fun attr ->
                 let location = attr |> Location.get in
                 let key, value = attr |> Location.get_data in
                 (key, value |> Location.add location))
          |> StringMap.of_seq
        in
        let self_closing = t |> optional Token.HTML_OR_COMPONENT_TAG_SELF_CLOSING in
        let children =
          if self_closing then
            []
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

  and parse_statement t =
    match t.token.typ with
    (* PARSING BREAK STATEMENT *)
    | Token.KEYWORD_BREAK ->
        next t;
        let _ = optional Token.SEMICOLON t in
        Some Ast.BreakStatement
    (* PARSING CONTINUE STATEMENT *)
    | Token.KEYWORD_CONTINUE ->
        next t;
        let _ = optional Token.SEMICOLON t in
        Some Ast.ContinueStatement
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
        Some (Ast.MutationStatement (Lowercase_Id identifier, expression))
    (* PARSING USE STATEMENT *)
    | Token.KEYWORD_USE ->
        next t;
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
        Some (Ast.UseStatement (Uppercase_Id identifier, expression))
    | _ ->
        let expr = parse_expression t in
        let _ = optional Token.SEMICOLON t in
        expr |> Option.map (fun expression -> Ast.ExpressionStatement expression)

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
          | Token.IDENT_LOWER _ ->
              let token = peek t in
              token = Token.COLON || token = Token.QUESTIONMARK
          | _ -> false
        in
        if is_record then (
          let attrs =
            t
            |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_record_field
            |> List.to_seq
            |> Seq.mapi (fun index attr ->
                   let location = attr |> Location.get in
                   let key, (optional, value) = attr |> Location.get_data in
                   (key, (index, optional, value) |> Location.add location))
            |> StringMap.of_seq
          in
          t |> expect Token.RIGHT_BRACE;
          Some Ast.(Record attrs))
        else (
          let statements = t |> Helpers.list ~fn:parse_statement in
          t |> expect Token.RIGHT_BRACE;
          Some (Ast.BlockExpression statements))
    (* PARSING FOR IN EXPRESSION *)
    | Token.KEYWORD_FOR -> (
        next t;
        expect Token.LEFT_PAREN t;
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
        expect Token.RIGHT_PAREN t;
        let body = parse_statement t in
        match body with
        | None ->
            Diagnostics.error t.token.location "Expected statement as body of for loop"
        | Some body ->
            Some
              (Ast.ForInExpression { index; iterator; reverse; iterable = expr1; body }))
    (* PARSING FN EXPRESSION *)
    | Token.KEYWORD_FN ->
        next t;
        expect Token.LEFT_PAREN t;
        let parameters =
          t |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_fn_param
        in
        expect Token.RIGHT_PAREN t;
        expect Token.ARROW t;
        let* body = t |> parse_expression in
        Some (Ast.Function (parameters, body))
    (* PARSING IF EXPRESSION *)
    | Token.KEYWORD_IF ->
        next t;
        expect Token.LEFT_PAREN t;
        let* condition = t |> parse_expression in
        expect Token.RIGHT_PAREN t;
        let* consequent = t |> parse_statement in
        let alternate =
          if optional Token.KEYWORD_ELSE t then
            t |> parse_statement
          else
            None
        in
        Some (Ast.ConditionalExpression { condition; consequent; alternate })
    (* PARSING TAG EXPRESSION *)
    | Token.TAG name ->
        next t;
        parse_tag ~name t
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
    (* PARSING IDENTIFIER EXPRESSION *)
    | Token.IDENT_LOWER identifier ->
        next t;
        Some (Ast.LowercaseIdentifierExpression identifier)
    | Token.IDENT_UPPER identifier ->
        next t;
        Some (Ast.UppercaseIdentifierExpression identifier)
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
          Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression t |> Array.of_list
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

  and parse_expression ?(prio = -999) t =
    let rec loop ~prio ~left t =
      match parse_binary_operator t with
      | None -> left
      | Some { precedence; _ } when precedence < prio -> left
      | Some { typ = Ast.Operators.Binary.FUNCTION_CALL; closing_token; _ } ->
          let name_location = Location.make ~s:t.token.location.loc_start in
          next t;
          let name_location = name_location ~e:t.token.location.loc_end in
          let arguments =
            t |> Helpers.separated_list ~sep:Token.COMMA ~fn:parse_expression
          in
          let left =
            Ast.FunctionCall (left |> Location.add (name_location ()), arguments)
          in
          let expect_close token = expect token t in
          let () = Option.iter expect_close closing_token in
          loop ~left ~prio t
      | Some { typ = operator; precedence; assoc; closing_token } -> (
          next t;
          let new_prio =
            match assoc with
            | Left -> precedence + 1
            | Right -> precedence
          in
          match parse_expression ~prio:new_prio t with
          | None ->
              Diagnostics.error
                t.token.location
                ("Expected expression on right hand side of `"
                ^ Ast.Operators.Binary.to_string operator
                ^ "`")
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
      | Token.KEYWORD_LIBRARY
      | Token.KEYWORD_STORE ) as typ -> (
        next t;
        let identifier = Helpers.expect_identifier ~typ:`Upper t in
        let attributes =
          if optional Token.LEFT_PAREN t then (
            let attributes =
              Helpers.separated_list ~sep:Token.COMMA ~fn:parse_attribute t
              |> List.to_seq
              |> Seq.map (fun attr ->
                     let location = attr |> Location.get in
                     let key, value = attr |> Location.get_data in
                     (key, value |> Location.add location))
              |> StringMap.of_seq
            in
            t |> expect Token.RIGHT_PAREN;
            Some attributes)
          else
            None
        in
        let body = t |> parse_expression in
        match body with
        | None -> Diagnostics.error t.token.location "Expected declaration to have a body"
        | Some body -> (
            match typ with
            | Token.KEYWORD_SITE ->
                Some (identifier, Ast.SiteDeclaration (attributes, body))
            | Token.KEYWORD_PAGE ->
                Some (identifier, Ast.PageDeclaration (attributes, body))
            | Token.KEYWORD_COMPONENT ->
                Some (identifier, Ast.ComponentDeclaration (attributes, body))
            | Token.KEYWORD_STORE ->
                Some (identifier, Ast.StoreDeclaration (attributes, body))
            | Token.KEYWORD_LIBRARY ->
                Some (identifier, Ast.LibraryDeclaration (attributes, body))
            | _ -> assert false))
    | Token.END_OF_INPUT -> None
    | _ -> assert false
  ;;
end

let scan : t -> Ast.t =
 fun t ->
  t
  |> Helpers.list ~fn:Rules.parse_declaration
  |> List.to_seq
  |> Seq.map (fun attr ->
         let location = attr |> Location.get in
         let key, value = attr |> Location.get_data in
         (key, value |> Location.add location))
  |> StringMap.of_seq
;;

let parse ~filename source =
  let parser = make ~filename source in
  scan parser
;;
