open PPrint
module Parsetree = Pinc_Parser.Parsetree

module Helpers = struct
  let comma_separated_attributes format = function
    | [] -> empty
    | lst ->
        let lst =
          List.map
            (fun (key, value) ->
              nest
                2
                (ifflat empty (break 1) ^^ string key ^^ colon ^^ blank 1 ^^ format value))
            lst
        in
        group (separate (comma ^^ space) lst ^^ ifflat empty (comma ^^ break 1))
  ;;

  let html_attributes format = function
    | [] -> empty
    | lst ->
        let lst =
          List.map
            (fun (key, value) ->
              nest 2 (ifflat empty (break 1) ^^ string key ^^ equals ^^ format value))
            lst
        in
        group (separate space lst)
  ;;
end

let rec format_annotations annotations = concat_map format_annotation annotations

and format_annotation = function
  | Parsetree.P_Comment_Annotation s -> format_comment s ^^ hardline
  | Parsetree.P_Blankline_Annotation i -> repeat i hardline

and format_lowercase_id = function
  | Parsetree.P_Lowercase_Id (id, _loc) -> string id

and format_uppercase_id = function
  | Parsetree.P_Uppercase_Id (id, _loc) -> string id

and format_comment comment =
  let comment = nest 2 (ifflat empty (break 1) ^^ arbitrary_string comment) in
  string "/*" ^^ group (comment ^^ ifflat empty (break 1)) ^^ string "*/"

and format_string templates =
  let templates =
    List.map
      (fun (t : Parsetree.string_template) ->
        match t.string_template_desc with
        | Parsetree.P_StringInterpolation (Parsetree.P_Lowercase_Id (id, _)) ->
            dollar ^^ parens (string id)
        | Parsetree.P_StringText s -> string s)
      templates
  in
  dquotes (concat templates)

and format_char c =
  let buf = Buffer.create 1 in
  Buffer.add_utf_8_uchar buf c;
  squotes (string (Buffer.contents buf))

and format_int i = string (string_of_int i)
and format_float f = string (string_of_float f)

and format_bool = function
  | true -> string "true"
  | false -> string "false"

and format_array a =
  let array =
    a |> List.map (fun expr -> nest 2 (ifflat empty (break 1) ^^ format_expression expr))
  in
  brackets @@ group (separate (comma ^^ space) array ^^ ifflat empty (comma ^^ break 1))

and format_record = function
  | [] -> braces empty
  | lst ->
      let lst =
        List.map
          (fun (key, (required, value)) ->
            nest
              2
              (hardline
              ^^ string key
              ^^ (match required with
                | `Optional -> qmark
                | `Required -> empty)
              ^^ colon
              ^^ blank 1
              ^^ format_expression value))
          lst
      in
      lbrace
      ^^ group (separate (comma ^^ space) lst ^^ ifflat empty (comma ^^ break 1))
      ^^ rbrace

and format_external_function parameters name =
  let parameters =
    let p =
      List.map
        (fun expr -> nest 2 (ifflat empty (break 1) ^^ format_lowercase_id expr))
        parameters
    in
    parens @@ group (separate (comma ^^ space) p ^^ ifflat empty (comma ^^ break 1))
  in
  string "fn"
  ^^ space
  ^^ parameters
  ^^ space
  ^^ minus
  ^^ rangle
  ^^ space
  ^^ twice percent
  ^^ string name
  ^^ twice percent

and format_function parameters body =
  let parameters =
    let p =
      List.map
        (fun expr -> nest 2 (ifflat empty (break 1) ^^ format_lowercase_id expr))
        parameters
    in
    parens @@ group (separate (comma ^^ space) p ^^ ifflat empty (comma ^^ break 1))
  in
  string "fn"
  ^^ space
  ^^ parameters
  ^^ space
  ^^ minus
  ^^ rangle
  ^^ space
  ^^ format_expression body

and format_function_call function_definition arguments =
  let arguments =
    match arguments with
    | [] -> empty
    | lst ->
        let lst =
          List.map
            (fun value -> nest 2 (ifflat empty (break 1) ^^ format_expression value))
            lst
        in
        group (separate (comma ^^ space) lst ^^ ifflat empty (comma ^^ break 1))
  in
  format_expression function_definition ^^ parens arguments

and format_uppercase_id_path_expression path =
  separate_map dot format_uppercase_id_expression path

and format_uppercase_id_expression id = string id
and format_lowercase_id_expression id = string id

and format_tag (tag : Parsetree.tag_desc) =
  let arguments =
    match tag.attributes with
    | [] -> empty
    | attrs -> parens @@ Helpers.comma_separated_attributes format_expression attrs
  in
  let transformer =
    match tag.transformer with
    | None -> empty
    | Some expr -> space ^^ repeat 2 colon ^^ space ^^ format_expression expr
  in
  sharp ^^ format_tag_kind tag.tag ^^ arguments ^^ transformer

and format_tag_kind = function
  | Parsetree.P_Tag_String -> string "String"
  | Parsetree.P_Tag_Int -> string "Int"
  | Parsetree.P_Tag_Float -> string "Float"
  | Parsetree.P_Tag_Boolean -> string "Boolean"
  | Parsetree.P_Tag_Array -> string "Array"
  | Parsetree.P_Tag_Record -> string "Record"
  | Parsetree.P_Tag_Slot -> string "Slot"
  | Parsetree.P_Tag_Store -> string "Store"
  | Parsetree.P_Tag_SetContext -> string "SetContext"
  | Parsetree.P_Tag_GetContext -> string "GetContext"
  | Parsetree.P_Tag_CreatePortal -> string "CreatePortal"
  | Parsetree.P_Tag_Portal -> string "Portal"
  | Parsetree.P_Tag_Custom s -> string s

and format_unary_expression op right =
  let r = format_expression right in
  match op with
  | Parsetree.Operators.Unary.MINUS -> minus ^^ r
  | Parsetree.Operators.Unary.NOT -> bang ^^ r

and format_binary_expression left op right =
  let l = format_expression left in
  let r = format_expression right in
  match op with
  | Parsetree.Operators.Binary.EQUAL -> l ^^ space ^^ repeat 2 equals ^^ space ^^ r
  | Parsetree.Operators.Binary.NOT_EQUAL -> l ^^ space ^^ bang ^^ equals ^^ space ^^ r
  | Parsetree.Operators.Binary.GREATER -> l ^^ space ^^ rangle ^^ space ^^ r
  | Parsetree.Operators.Binary.GREATER_EQUAL ->
      l ^^ space ^^ rangle ^^ equals ^^ space ^^ r
  | Parsetree.Operators.Binary.LESS -> l ^^ space ^^ langle ^^ space ^^ r
  | Parsetree.Operators.Binary.LESS_EQUAL -> l ^^ space ^^ langle ^^ equals ^^ space ^^ r
  | Parsetree.Operators.Binary.PLUS -> l ^^ space ^^ plus ^^ space ^^ r
  | Parsetree.Operators.Binary.MINUS -> l ^^ space ^^ minus ^^ space ^^ r
  | Parsetree.Operators.Binary.TIMES -> l ^^ space ^^ star ^^ space ^^ r
  | Parsetree.Operators.Binary.DIV -> l ^^ space ^^ slash ^^ space ^^ r
  | Parsetree.Operators.Binary.POW -> l ^^ space ^^ repeat 2 star ^^ space ^^ r
  | Parsetree.Operators.Binary.MODULO -> l ^^ space ^^ percent ^^ space ^^ r
  | Parsetree.Operators.Binary.CONCAT -> l ^^ space ^^ repeat 2 plus ^^ space ^^ r
  | Parsetree.Operators.Binary.AND -> l ^^ space ^^ repeat 2 ampersand ^^ space ^^ r
  | Parsetree.Operators.Binary.OR -> l ^^ space ^^ repeat 2 bar ^^ space ^^ r
  | Parsetree.Operators.Binary.DOT_ACCESS -> l ^^ dot ^^ r
  | Parsetree.Operators.Binary.BRACKET_ACCESS -> l ^^ brackets r
  | Parsetree.Operators.Binary.FUNCTION_CALL -> l ^^ parens r
  | Parsetree.Operators.Binary.PIPE -> l ^^ space ^^ bar ^^ rangle ^^ space ^^ r
  | Parsetree.Operators.Binary.ARRAY_ADD -> l ^^ space ^^ at ^^ space ^^ r
  | Parsetree.Operators.Binary.MERGE -> l ^^ space ^^ repeat 2 at ^^ space ^^ r
  | Parsetree.Operators.Binary.RANGE -> l ^^ repeat 2 dot ^^ r
  | Parsetree.Operators.Binary.INCLUSIVE_RANGE -> l ^^ repeat 3 dot ^^ r

and format_for_in ~index ~iterator ~reverse ~iterable ~body =
  string "for"
  ^^ space
  ^^ lparen
  ^^ optional format_lowercase_id index
  ^^ optional (Fun.const comma) index
  ^^ optional (Fun.const space) index
  ^^ format_lowercase_id iterator
  ^^ space
  ^^ string "in"
  ^^ space
  ^^ (if reverse then
        string "reverse" ^^ space
      else
        empty)
  ^^ format_expression iterable
  ^^ rparen
  ^^ space
  ^^ format_expression body

and format_conditional ~condition ~consequent ~alternate =
  string "if"
  ^^ space
  ^^ parens (format_expression condition)
  ^^ space
  ^^ format_expression consequent
  ^^ optional
       (fun expr -> space ^^ string "else" ^^ space ^^ format_expression expr)
       alternate

and format_block statements =
  let statements = List.map (fun s -> format_statement s) statements in
  lbrace ^^ nest 2 (break 1 ^^ separate hardline statements) ^^ break 1 ^^ rbrace

and format_html_template_node
    ~html_tag_identifier
    ~html_tag_attributes
    ~html_tag_children
    ~html_tag_self_closing:_ =
  langle
  ^^ string html_tag_identifier
  ^^ (match html_tag_attributes with
    | [] -> empty
    | attrs -> space ^^ Helpers.html_attributes format_expression attrs)
  ^^
  match html_tag_children with
  | [] -> space ^^ slash ^^ rangle
  | children ->
      rangle
      ^^ concat_map format_template_node children
      ^^ langle
      ^^ slash
      ^^ string html_tag_identifier
      ^^ rangle

and format_fragment_template_node ~fragement_children =
  langle
  ^^ rangle
  ^^ concat_map format_template_node fragement_children
  ^^ langle
  ^^ rangle

and format_component_template_node
    ~component_tag_identifier
    ~component_tag_attributes
    ~component_tag_children =
  langle
  ^^ format_uppercase_id component_tag_identifier
  ^^ (match component_tag_attributes with
    | [] -> empty
    | attrs -> space ^^ Helpers.html_attributes format_expression attrs)
  ^^
  match component_tag_children with
  | [] -> space ^^ slash ^^ rangle
  | children ->
      rangle
      ^^ concat_map format_template_node children
      ^^ langle
      ^^ slash
      ^^ format_uppercase_id component_tag_identifier
      ^^ rangle

and format_expression_template_node ~template_expression_node_expression =
  braces (format_expression template_expression_node_expression)

and format_text_template_node ~text_template_node_text = string text_template_node_text

and format_template_node (node : Parsetree.template_node) =
  match node.template_node_desc with
  | P_HtmlTemplateNode
      {
        html_tag_identifier;
        html_tag_attributes;
        html_tag_children;
        html_tag_self_closing;
      } ->
      format_html_template_node
        ~html_tag_identifier
        ~html_tag_attributes
        ~html_tag_children
        ~html_tag_self_closing
  | P_ComponentTemplateNode
      { component_tag_identifier; component_tag_attributes; component_tag_children } ->
      format_component_template_node
        ~component_tag_identifier
        ~component_tag_attributes
        ~component_tag_children
  | P_FragmentTemplateNode { fragement_children } ->
      format_fragment_template_node ~fragement_children
  | P_ExpressionTemplateNode { template_expression_node_expression } ->
      format_expression_template_node ~template_expression_node_expression
  | P_TextTemplateNode { text_template_node_text } ->
      format_text_template_node ~text_template_node_text

and format_expression (exression : Parsetree.expression) =
  match exression.expression_desc with
  | P_Comment comment -> format_comment comment
  | P_String templates -> format_string templates
  | P_Char c -> format_char c
  | P_Int i -> format_int i
  | P_Float f -> format_float f
  | P_Bool b -> format_bool b
  | P_Array array -> format_array array
  | P_Record record -> format_record record
  | P_ExternalFunction { parameters; name } -> format_external_function parameters name
  | P_Function { parameters; body } -> format_function parameters body
  | P_FunctionCall { function_definition; arguments } ->
      format_function_call function_definition arguments
  | P_UppercaseIdentifierPathExpression path -> format_uppercase_id_path_expression path
  | P_UppercaseIdentifierExpression id -> format_uppercase_id_expression id
  | P_LowercaseIdentifierExpression id -> format_lowercase_id_expression id
  | P_TagExpression { tag_desc; tag_loc = _ } -> format_tag tag_desc
  | P_ForInExpression { index; iterator; reverse; iterable; body } ->
      format_for_in ~index ~iterator ~reverse ~iterable ~body
  | P_ConditionalExpression { condition; consequent; alternate } ->
      format_conditional ~condition ~consequent ~alternate
  | P_BlockExpression statements -> format_block statements
  | P_TemplateExpression node -> format_template_node node
  | P_UnaryExpression (op, expr) -> format_unary_expression op expr
  | P_BinaryExpression (left, op, right) -> format_binary_expression left op right

and format_comment_stmt s = format_comment s

and format_break_stmt i =
  let num =
    if i = 1 then
      empty
    else
      space ^^ string (string_of_int i)
  in
  string "break" ^^ num ^^ semi

and format_continue_stmt i =
  let num =
    if i = 1 then
      empty
    else
      space ^^ string (string_of_int i)
  in
  string "continue" ^^ num ^^ semi

and format_use_stmt id expr =
  string "use"
  ^^ space
  ^^ (match id with
    | None -> empty
    | Some id -> format_uppercase_id id ^^ space ^^ equals ^^ space)
  ^^ format_expression expr
  ^^ semi

and format_optional_mutable_let id expr =
  string "let"
  ^^ space
  ^^ string "mutable"
  ^^ space
  ^^ format_lowercase_id id
  ^^ qmark
  ^^ space
  ^^ equals
  ^^ space
  ^^ format_expression expr
  ^^ semi

and format_optional_let id expr =
  string "let"
  ^^ space
  ^^ format_lowercase_id id
  ^^ qmark
  ^^ space
  ^^ equals
  ^^ space
  ^^ format_expression expr
  ^^ semi

and format_mutable_let id expr =
  string "let"
  ^^ space
  ^^ string "mutable"
  ^^ space
  ^^ format_lowercase_id id
  ^^ space
  ^^ equals
  ^^ space
  ^^ format_expression expr
  ^^ semi

and format_let id expr =
  string "let"
  ^^ space
  ^^ format_lowercase_id id
  ^^ space
  ^^ equals
  ^^ space
  ^^ format_expression expr
  ^^ semi

and format_mutation id expr =
  format_lowercase_id id
  ^^ space
  ^^ colon
  ^^ equals
  ^^ space
  ^^ format_expression expr
  ^^ semi

and format_expression_stmt expr = format_expression expr

and format_statement (statement : Parsetree.statement) =
  match statement.statement_desc with
  | P_CommentStatement s -> format_comment_stmt s
  | P_BreakStatement s -> format_break_stmt s
  | P_ContinueStatement s -> format_continue_stmt s
  | P_UseStatement (id, expr) -> format_use_stmt id expr
  | P_OptionalMutableLetStatement (id, expr) -> format_optional_mutable_let id expr
  | P_OptionalLetStatement (id, expr) -> format_optional_let id expr
  | P_MutableLetStatement (id, expr) -> format_mutable_let id expr
  | P_LetStatement (id, expr) -> format_let id expr
  | P_MutationStatement (id, expr) -> format_mutation id expr
  | P_ExpressionStatement s -> format_expression_stmt s

and format_declaration key (declaration : Parsetree.declaration) =
  let annotations = format_annotations declaration.declaration_annotations in
  let typ =
    match declaration.declaration_kind with
    | P_Declaration_Component -> string "component"
    | P_Declaration_Library -> string "library"
    | P_Declaration_Page -> string "page"
    | P_Declaration_Store -> string "store"
  in
  let attributes =
    Helpers.comma_separated_attributes
      format_expression
      declaration.declaration_attributes
  in
  let body = format_expression declaration.declaration_body in
  annotations ^^ typ ^^ blank 1 ^^ string key ^^ parens attributes ^^ blank 1 ^^ body

and format_declarations declarations =
  let declarations =
    declarations
    |> List.map (fun (key, declaration) -> format_declaration key declaration)
  in

  separate (hardline ^^ hardline) declarations
;;

let format (declarations : Pinc_Parser.parsetree) : string =
  let doc = format_declarations declarations in
  let buf = Buffer.create 65565 in
  PPrint.ToBuffer.pretty 1. 100 buf doc;
  Buffer.contents buf
;;
