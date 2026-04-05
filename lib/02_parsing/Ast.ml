module Operators = Operators

type uppercase_identifier = Uppercase_Id of (string * Pinc_Diagnostics.Location.t)
and lowercase_identifier = Lowercase_Id of (string * Pinc_Diagnostics.Location.t)

and template_node = {
  template_node_loc : Pinc_Diagnostics.Location.t;
  template_node_desc : template_node_desc;
}

and template_node_desc =
  | HtmlTemplateNode of {
      html_tag_identifier : string;
      html_tag_attributes : expression StringMap.t;
      html_tag_children : template_node list;
      html_tag_self_closing : bool;
    }
  | ComponentTemplateNode of {
      component_tag_identifier : uppercase_identifier;
      component_tag_attributes : expression StringMap.t;
      component_tag_children : template_node list;
    }
  | FragmentTemplateNode of { fragement_children : template_node list }
  | ExpressionTemplateNode of { template_expression_node_expression : expression }
  | TextTemplateNode of { text_template_node_text : string }

and tag = {
  tag_loc : Pinc_Diagnostics.Location.t;
  tag_desc : tag_desc;
}

and tag_kind =
  | Tag_String
  | Tag_Int
  | Tag_Float
  | Tag_Boolean
  | Tag_Array
  | Tag_Record
  | Tag_Slot
  | Tag_Store
  | Tag_SetContext
  | Tag_GetContext
  | Tag_CreatePortal
  | Tag_Portal
  | Tag_Custom of string

and tag_desc = {
  tag : tag_kind;
  key : string;
  required : bool;
  attributes : expression StringMap.t;
  transformer : expression option;
  children : expression option;
}

and string_template = {
  string_template_loc : Pinc_Diagnostics.Location.t;
  string_template_desc : string_template_desc;
}

and string_template_desc =
  | StringInterpolation of lowercase_identifier
  | StringText of string

and expression = {
  expression_loc : Pinc_Diagnostics.Location.t;
  expression_desc : expression_desc;
}

and expression_desc =
  | Comment of string
  | String of string_template list
  | Char of Uchar.t
  | Int of int
  | Float of float
  | Bool of bool
  | Array of expression array
  | Record of ([ `Required | `Optional ] * expression) StringMap.t
  | ExternalFunction of {
      identifier : lowercase_identifier;
      parameters : lowercase_identifier list;
      name : string;
    }
  | Function of {
      identifier : lowercase_identifier option;
      parameters : lowercase_identifier list;
      body : expression;
    }
  | FunctionCall of {
      function_definition : expression;
      arguments : expression list;
    }
  | UppercaseIdentifierPathExpression of string list
  | UppercaseIdentifierExpression of string
  | LowercaseIdentifierExpression of string
  | TagExpression of tag
  | ForInExpression of {
      index : lowercase_identifier option;
      iterator : lowercase_identifier;
      reverse : bool;
      iterable : expression;
      body : expression;
    }
  | TemplateExpression of template_node
  | BlockExpression of statement list
  | ConditionalExpression of {
      condition : expression;
      consequent : expression;
      alternate : expression option;
    }
  | UnaryExpression of Operators.Unary.t * expression
  | BinaryExpression of expression * Operators.Binary.t * expression

and statement = {
  statement_loc : Pinc_Diagnostics.Location.t;
  statement_desc : statement_desc;
}

and statement_desc =
  | CommentStatement of string
  | BreakStatement of int
  | ContinueStatement of int
  | UseStatement of uppercase_identifier option * expression
  | OptionalMutableLetStatement of lowercase_identifier * expression
  | OptionalLetStatement of lowercase_identifier * expression
  | MutableLetStatement of lowercase_identifier * expression
  | LetStatement of lowercase_identifier * expression
  | MutationStatement of lowercase_identifier * expression
  | ExpressionStatement of expression

and declaration = {
  declaration_loc : Pinc_Diagnostics.Location.t;
  declaration_kind : declaration_kind;
  declaration_attributes : expression StringMap.t;
  declaration_body : expression;
}

and declaration_kind =
  | Declaration_Component
  | Declaration_Library
  | Declaration_Page
  | Declaration_Store

and t = declaration StringMap.t

module Declaration = struct
  let marshal (d : declaration) = Marshal.to_string d []

  let unmarshal s =
    let result : declaration = Marshal.from_string s 0 in
    result
  ;;
end

let show_expression expr =
  match expr with
  | { expression_loc = _; expression_desc = Comment _ } -> "Comment"
  | { expression_loc = _; expression_desc = String _ } -> "String"
  | { expression_loc = _; expression_desc = Char _ } -> "Char"
  | { expression_loc = _; expression_desc = Int _ } -> "Int"
  | { expression_loc = _; expression_desc = Float _ } -> "Float"
  | { expression_loc = _; expression_desc = Bool _ } -> "Bool"
  | { expression_loc = _; expression_desc = Array _ } -> "Array"
  | { expression_loc = _; expression_desc = Record _ } -> "Record"
  | { expression_loc = _; expression_desc = ExternalFunction _ } -> "ExternalFunction"
  | { expression_loc = _; expression_desc = Function _ } -> "Function"
  | { expression_loc = _; expression_desc = FunctionCall _ } -> "FunctionCall"
  | { expression_loc = _; expression_desc = UppercaseIdentifierPathExpression _ } ->
      "UppercaseIdentifierPathExpression"
  | { expression_loc = _; expression_desc = UppercaseIdentifierExpression _ } ->
      "UppercaseIdentifierExpression"
  | { expression_loc = _; expression_desc = LowercaseIdentifierExpression _ } ->
      "LowercaseIdentifierExpression"
  | { expression_loc = _; expression_desc = TagExpression _ } -> "TagExpression"
  | { expression_loc = _; expression_desc = ForInExpression _ } -> "ForInExpression"
  | { expression_loc = _; expression_desc = TemplateExpression _ } -> "TemplateExpression"
  | { expression_loc = _; expression_desc = BlockExpression _ } -> "BlockExpression"
  | { expression_loc = _; expression_desc = ConditionalExpression _ } ->
      "ConditionalExpression"
  | { expression_loc = _; expression_desc = UnaryExpression _ } -> "UnaryExpression"
  | { expression_loc = _; expression_desc = BinaryExpression _ } -> "BinaryExpression"
;;
