open Ast

module Env = struct
  type t = { current_identifier : ([ `Required | `Optional ] * string) option }
end

let rec transform_lowercase_id = function
  | Parsetree.P_Lowercase_Id (id, loc) -> Lowercase_Id (id, loc)

and transform_uppercase_id = function
  | Parsetree.P_Uppercase_Id (id, loc) -> Uppercase_Id (id, loc)

and transform_comment comment = Comment comment

and transform_string templates =
  let transform_string_template (template : Parsetree.string_template) =
    {
      string_template_loc = template.string_template_loc;
      string_template_desc =
        (match template.string_template_desc with
        | P_StringInterpolation identifier ->
            StringInterpolation (transform_lowercase_id identifier)
        | P_StringText s -> StringText s);
    }
  in
  String (List.map transform_string_template templates)

and transform_char c = Char c
and transform_int i = Int i
and transform_float f = Float f
and transform_bool b = Bool b
and transform_array a = Array (Array.map transform_expression a)

and transform_record r =
  let r =
    StringMap.map (fun (requirement, expr) -> (requirement, transform_expression expr)) r
  in
  Record r

and transform_external_function parameters name = ExternalFunction { parameters; name }

and transform_function parameters body =
  Function { parameters; body = transform_expression body }

and transform_function_call function_definition arguments =
  FunctionCall
    {
      function_definition = transform_expression function_definition;
      arguments = List.map transform_expression arguments;
    }

and transform_uppercase_id_path_expression path = UppercaseIdentifierPathExpression path
and transform_uppercase_id_expression id = UppercaseIdentifierExpression id
and transform_lowercase_id_expression id = LowercaseIdentifierExpression id

and transform_tag (tag : Parsetree.tag) =
  let transform_tag_desc (desc : Parsetree.tag_desc) : Ast.tag_desc =
    let tag =
      match desc.tag with
      | P_Tag_String -> Tag_String
      | P_Tag_Int -> Tag_Int
      | P_Tag_Float -> Tag_Float
      | P_Tag_Boolean -> Tag_Boolean
      | P_Tag_Array -> Tag_Array
      | P_Tag_Record -> Tag_Record
      | P_Tag_Slot -> Tag_Slot
      | P_Tag_Store -> Tag_Store
      | P_Tag_SetContext -> Tag_SetContext
      | P_Tag_GetContext -> Tag_GetContext
      | P_Tag_CreatePortal -> Tag_CreatePortal
      | P_Tag_Portal -> Tag_Portal
      | P_Tag_Custom s -> Tag_Custom s
    in
    let attributes = StringMap.map transform_expression desc.attributes in
    let transformer = Option.map transform_expression desc.transformer in

    { tag; attributes; transformer }
  in
  TagExpression { tag_loc = tag.tag_loc; tag_desc = transform_tag_desc tag.tag_desc }

and transform_unary_expression op right = UnaryExpression (op, transform_expression right)

and transform_binary_expression left op right =
  BinaryExpression (transform_expression left, op, transform_expression right)

and transform_for_in ~index ~iterator ~reverse ~iterable ~body =
  let index = Option.map transform_lowercase_id index in
  let iterator = transform_lowercase_id iterator in
  let reverse = reverse in
  let iterable = transform_expression iterable in
  let body = transform_expression body in
  ForInExpression { index; iterator; reverse; iterable; body }

and transform_conditional ~condition ~consequent ~alternate =
  let condition = transform_expression condition in
  let consequent = transform_statement consequent in
  let alternate = Option.map transform_statement alternate in
  ConditionalExpression { condition; consequent; alternate }

and transform_block statements =
  let statements = List.map transform_statement statements in
  BlockExpression statements

and transform_html_template_node
    ~html_tag_identifier
    ~html_tag_attributes
    ~html_tag_children
    ~html_tag_self_closing =
  let html_tag_identifier = html_tag_identifier in
  let html_tag_attributes = StringMap.map transform_expression html_tag_attributes in
  let html_tag_children = List.map transform_template_node html_tag_children in
  let html_tag_self_closing = html_tag_self_closing in
  HtmlTemplateNode
    { html_tag_identifier; html_tag_attributes; html_tag_children; html_tag_self_closing }

and transform_component_template_node
    ~component_tag_identifier
    ~component_tag_attributes
    ~component_tag_children =
  let component_tag_identifier = transform_uppercase_id component_tag_identifier in
  let component_tag_attributes =
    StringMap.map transform_expression component_tag_attributes
  in
  let component_tag_children = List.map transform_template_node component_tag_children in
  ComponentTemplateNode
    { component_tag_identifier; component_tag_attributes; component_tag_children }

and transform_expression_template_node expr =
  let expr = transform_expression expr in
  ExpressionTemplateNode expr

and transform_text_template_node s = TextTemplateNode s

and transform_template_node (node : Parsetree.template_node) =
  {
    template_node_loc = node.template_node_loc;
    template_node_desc =
      (match node.template_node_desc with
      | P_HtmlTemplateNode
          {
            html_tag_identifier;
            html_tag_attributes;
            html_tag_children;
            html_tag_self_closing;
          } ->
          transform_html_template_node
            ~html_tag_identifier
            ~html_tag_attributes
            ~html_tag_children
            ~html_tag_self_closing
      | P_ComponentTemplateNode
          { component_tag_identifier; component_tag_attributes; component_tag_children }
        ->
          transform_component_template_node
            ~component_tag_identifier
            ~component_tag_attributes
            ~component_tag_children
      | P_ExpressionTemplateNode expr -> transform_expression_template_node expr
      | P_TextTemplateNode s -> transform_text_template_node s);
  }

and transform_template nodes =
  let nodes = List.map transform_template_node nodes in
  TemplateExpression nodes

and transform_expression (exression : Parsetree.expression) =
  {
    expression_loc = exression.expression_loc;
    expression_desc =
      (match exression.expression_desc with
      | P_Comment comment -> transform_comment comment
      | P_String templates -> transform_string templates
      | P_Char c -> transform_char c
      | P_Int i -> transform_int i
      | P_Float f -> transform_float f
      | P_Bool b -> transform_bool b
      | P_Array array -> transform_array array
      | P_Record record -> transform_record record
      | P_ExternalFunction { parameters; name } ->
          transform_external_function parameters name
      | P_Function { parameters; body } -> transform_function parameters body
      | P_FunctionCall { function_definition; arguments } ->
          transform_function_call function_definition arguments
      | P_UppercaseIdentifierPathExpression path ->
          transform_uppercase_id_path_expression path
      | P_UppercaseIdentifierExpression id -> transform_uppercase_id_expression id
      | P_LowercaseIdentifierExpression id -> transform_lowercase_id_expression id
      | P_TagExpression tag -> transform_tag tag
      | P_ForInExpression { index; iterator; reverse; iterable; body } ->
          transform_for_in ~index ~iterator ~reverse ~iterable ~body
      | P_ConditionalExpression { condition; consequent; alternate } ->
          transform_conditional ~condition ~consequent ~alternate
      | P_BlockExpression statements -> transform_block statements
      | P_TemplateExpression nodes -> transform_template nodes
      | P_UnaryExpression (op, expr) -> transform_unary_expression op expr
      | P_BinaryExpression (left, op, right) -> transform_binary_expression left op right);
  }

and transform_comment_stmt s = CommentStatement s
and transform_break_stmt s = BreakStatement s
and transform_continue_stmt s = ContinueStatement s

and transform_use_stmt id expr =
  let id = Option.map transform_uppercase_id id in
  let expr = transform_expression expr in
  UseStatement (id, expr)

and transform_optional_mutable_let id expr =
  let id = transform_lowercase_id id in
  let expr = transform_expression expr in
  OptionalMutableLetStatement (id, expr)

and transform_optional_let id expr =
  let id = transform_lowercase_id id in
  let expr = transform_expression expr in
  OptionalLetStatement (id, expr)

and transform_mutable_let id expr =
  let id = transform_lowercase_id id in
  let expr = transform_expression expr in
  MutableLetStatement (id, expr)

and transform_let id expr =
  let id = transform_lowercase_id id in
  let expr = transform_expression expr in
  LetStatement (id, expr)

and transform_mutation id expr =
  let id = transform_lowercase_id id in
  let expr = transform_expression expr in
  MutationStatement (id, expr)

and transform_expression_stmt s = ExpressionStatement (transform_expression s)

and transform_statement (statement : Parsetree.statement) =
  {
    statement_loc = statement.statement_loc;
    statement_desc =
      (match statement.statement_desc with
      | P_CommentStatement s -> transform_comment_stmt s
      | P_BreakStatement s -> transform_break_stmt s
      | P_ContinueStatement s -> transform_continue_stmt s
      | P_UseStatement (id, expr) -> transform_use_stmt id expr
      | P_OptionalMutableLetStatement (id, expr) -> transform_optional_mutable_let id expr
      | P_OptionalLetStatement (id, expr) -> transform_optional_let id expr
      | P_MutableLetStatement (id, expr) -> transform_mutable_let id expr
      | P_LetStatement (id, expr) -> transform_let id expr
      | P_MutationStatement (id, expr) -> transform_mutation id expr
      | P_ExpressionStatement s -> transform_expression_stmt s);
  }

and transform_declaration (declaration : Parsetree.declaration) =
  {
    declaration_loc = declaration.declaration_loc;
    declaration_type =
      (match declaration.declaration_type with
      | P_Declaration_Component desc ->
          Declaration_Component
            {
              declaration_attributes =
                StringMap.map transform_expression desc.declaration_attributes;
              declaration_body = transform_expression desc.declaration_body;
            }
      | P_Declaration_Library desc ->
          Declaration_Library
            {
              declaration_attributes =
                StringMap.map transform_expression desc.declaration_attributes;
              declaration_body = transform_expression desc.declaration_body;
            }
      | P_Declaration_Page desc ->
          Declaration_Page
            {
              declaration_attributes =
                StringMap.map transform_expression desc.declaration_attributes;
              declaration_body = transform_expression desc.declaration_body;
            }
      | P_Declaration_Store desc ->
          Declaration_Store
            {
              declaration_attributes =
                StringMap.map transform_expression desc.declaration_attributes;
              declaration_body = transform_expression desc.declaration_body;
            });
  }

and transform_declarations declarations = StringMap.map transform_declaration declarations

let transform (declarations : Parsetree.t) : Ast.t = transform_declarations declarations
