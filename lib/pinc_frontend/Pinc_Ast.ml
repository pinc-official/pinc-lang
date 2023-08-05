module Location = Pinc_Diagnostics.Location
module Operators = Pinc_Operators

type uppercase_identifier = Uppercase_Id of (string * Location.t)
and lowercase_identifier = Lowercase_Id of (string * Location.t)

and template_node = {
  template_node_loc : Location.t;
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
  | ExpressionTemplateNode of expression
  | TextTemplateNode of string

and tag = {
  tag_loc : Location.t;
  tag_desc : tag_desc;
}

and tag_desc = {
  tag :
    [ `String
    | `Int
    | `Float
    | `Boolean
    | `Array
    | `Record
    | `Slot
    | `SetContext
    | `GetContext
    | `CreatePortal
    | `Portal
    | `Custom of string
    ];
  attributes : expression StringMap.t option;
  transformer : (lowercase_identifier * expression) option;
}

and string_template = {
  string_template_loc : Location.t;
  string_template_desc : string_template_desc;
}

and string_template_desc =
  | StringInterpolation of expression
  | StringText of string

and expression = {
  expression_loc : Location.t;
  expression_desc : expression_desc;
}

and expression_desc =
  | Comment
  | String of string_template list
  | Char of Uchar.t
  | Int of int
  | Float of float
  | Bool of bool
  | Array of expression array
  | Record of (int * bool * expression) StringMap.t  (** order, nullable, value *)
  | Function of {
      parameters : string list;
      body : expression;
    }
  | FunctionCall of {
      function_definition : expression;
      arguments : expression list;
    }
  | UppercaseIdentifierExpression of string
  | LowercaseIdentifierExpression of string
  | TagExpression of tag
  | ForInExpression of {
      index : lowercase_identifier option;
      iterator : lowercase_identifier;
      reverse : bool;
      iterable : expression;
      body : statement;
    }
  | TemplateExpression of template_node list
  | BlockExpression of statement list
  | ConditionalExpression of {
      condition : expression;
      consequent : statement;
      alternate : statement option;
    }
  | UnaryExpression of Operators.Unary.typ * expression
  | BinaryExpression of expression * Operators.Binary.typ * expression

and statement = {
  statement_loc : Location.t;
  statement_desc : statement_desc;
}

and statement_desc =
  | BreakStatement of int
  | ContinueStatement of int
  | UseStatement of uppercase_identifier * expression
  | OptionalMutableLetStatement of lowercase_identifier * expression
  | OptionalLetStatement of lowercase_identifier * expression
  | MutableLetStatement of lowercase_identifier * expression
  | LetStatement of lowercase_identifier * expression
  | MutationStatement of lowercase_identifier * expression
  | ExpressionStatement of expression

and declaration = {
  declaration_loc : Location.t;
  declaration_type : declaration_type;
}

and declaration_type =
  | Declaration_Component of declaration_desc
  | Declaration_Library of declaration_desc
  | Declaration_Site of declaration_desc
  | Declaration_Page of declaration_desc
  | Declaration_Store of declaration_desc

and declaration_desc = {
  declaration_attributes : expression StringMap.t option;
  declaration_body : expression;
}

and t = declaration StringMap.t

module Declaration = struct
  let marshal (d : declaration) = Marshal.to_string d []

  let unmarshal s =
    let result : declaration = Marshal.from_string s 0 in
    result
  ;;
end
