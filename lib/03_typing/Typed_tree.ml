open Types
module Operators = Pinc_Parser.Ast.Operators

type location = Pinc_Diagnostics.Location.t

type uppercase_identifier = T_Uppercase_Id of (string * Type.t * location)
and lowercase_identifier = T_Lowercase_Id of (string * Type.t * location)

and template_node = {
  template_node_loc : location;
  template_node_type : Type.t;
  template_node_desc : template_node_desc;
}

and template_node_desc =
  | T_HtmlTemplateNode of {
      html_tag_identifier : string;
      html_tag_attributes : expression StringMap.t;
      html_tag_children : template_node list;
      html_tag_self_closing : bool;
    }
  | T_ComponentTemplateNode of {
      component_tag_identifier : uppercase_identifier;
      component_tag_attributes : expression StringMap.t;
      component_tag_children : template_node list;
    }
  | T_ExpressionTemplateNode of expression
  | T_TextTemplateNode of string

and tag = {
  tag_loc : location;
  tag_type : Type.t;
  tag_desc : tag_desc;
}

and tag_kind =
  | T_Tag_String
  | T_Tag_Int
  | T_Tag_Float
  | T_Tag_Boolean
  | T_Tag_Array
  | T_Tag_Record
  | T_Tag_Slot
  | T_Tag_Store
  | T_Tag_SetContext
  | T_Tag_GetContext
  | T_Tag_CreatePortal
  | T_Tag_Portal
  | T_Tag_Custom of string

and tag_desc = {
  tag : tag_kind;
  key : string;
  required : bool;
  attributes : expression StringMap.t;
  transformer : expression option;
  children : expression option;
}

and string_template = {
  string_template_loc : location;
  string_template_type : Type.t;
  string_template_desc : string_template_desc;
}

and string_template_desc =
  | T_StringInterpolation of lowercase_identifier
  | T_StringText of string

and expression = {
  expression_loc : location;
  expression_type : Type.t;
  expression_desc : expression_desc;
}

and expression_desc =
  | T_Comment of string
  | T_String of string_template list
  | T_Char of Uchar.t
  | T_Int of int
  | T_Float of float
  | T_Bool of bool
  | T_Array of expression array
  | T_Record of ([ `Required | `Optional ] * expression) StringMap.t
  | T_ExternalFunction of {
      identifier : lowercase_identifier;
      parameters : lowercase_identifier list;
      name : string;
    }
  | T_Function of {
      identifier : lowercase_identifier option;
      parameters : lowercase_identifier list;
      body : expression;
    }
  | T_FunctionCall of {
      function_definition : expression;
      arguments : expression list;
    }
  | T_UppercaseIdentifierPathExpression of string list
  | T_UppercaseIdentifierExpression of uppercase_identifier
  | T_LowercaseIdentifierExpression of lowercase_identifier
  | T_TagExpression of tag
  | T_ForInExpression of {
      index : lowercase_identifier option;
      iterator : lowercase_identifier;
      reverse : bool;
      iterable : expression;
      body : expression;
    }
  | T_TemplateExpression of template_node list
  | T_BlockExpression of statement list
  | T_ConditionalExpression of {
      condition : expression;
      consequent : statement;
      alternate : statement option;
    }
  | T_UnaryExpression of Operators.Unary.t * expression
  | T_BinaryExpression of expression * Operators.Binary.t * expression

and statement = {
  statement_loc : location;
  statement_type : Type.t;
  statement_desc : statement_desc;
}

and statement_desc =
  | T_CommentStatement of string
  | T_BreakStatement of int
  | T_ContinueStatement of int
  | T_UseStatement of uppercase_identifier option * expression
  | T_OptionalMutableLetStatement of lowercase_identifier * expression
  | T_OptionalLetStatement of lowercase_identifier * expression
  | T_MutableLetStatement of lowercase_identifier * expression
  | T_LetStatement of lowercase_identifier * expression
  | T_MutationStatement of lowercase_identifier * expression
  | T_ExpressionStatement of expression

and declaration = {
  declaration_loc : location;
  declaration_type : Type.t;
  declaration_kind : declaration_kind;
}

and declaration_kind =
  | T_Declaration_Component of declaration_desc
  | T_Declaration_Library of declaration_desc
  | T_Declaration_Page of declaration_desc
  | T_Declaration_Store of declaration_desc

and declaration_desc = {
  declaration_attributes : expression StringMap.t;
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
