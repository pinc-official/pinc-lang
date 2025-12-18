type uppercase_identifier = P_Uppercase_Id of (string * Pinc_Diagnostics.Location.t)
and lowercase_identifier = P_Lowercase_Id of (string * Pinc_Diagnostics.Location.t)

and template_node = {
  template_node_loc : Pinc_Diagnostics.Location.t;
  template_node_desc : template_node_desc;
}

and template_node_desc =
  | P_HtmlTemplateNode of {
      html_tag_identifier : string;
      html_tag_attributes : expression StringMap.t;
      html_tag_children : template_node list;
      html_tag_self_closing : bool;
    }
  | P_ComponentTemplateNode of {
      component_tag_identifier : uppercase_identifier;
      component_tag_attributes : expression StringMap.t;
      component_tag_children : template_node list;
    }
  | P_ExpressionTemplateNode of expression
  | P_TextTemplateNode of string

and tag = {
  tag_loc : Pinc_Diagnostics.Location.t;
  tag_desc : tag_desc;
}

and tag_typ =
  | P_Tag_String
  | P_Tag_Int
  | P_Tag_Float
  | P_Tag_Boolean
  | P_Tag_Array
  | P_Tag_Record
  | P_Tag_Slot
  | P_Tag_Store
  | P_Tag_SetContext
  | P_Tag_GetContext
  | P_Tag_CreatePortal
  | P_Tag_Portal
  | P_Tag_Custom of string

and tag_desc = {
  tag : tag_typ;
  attributes : expression StringMap.t;
  transformer : expression option;
}

and string_template = {
  string_template_loc : Pinc_Diagnostics.Location.t;
  string_template_desc : string_template_desc;
}

and string_template_desc =
  | P_StringInterpolation of lowercase_identifier
  | P_StringText of string

and expression = {
  expression_loc : Pinc_Diagnostics.Location.t;
  expression_desc : expression_desc;
}

and expression_desc =
  | P_Comment of string
  | P_String of string_template list
  | P_Char of Uchar.t
  | P_Int of int
  | P_Float of float
  | P_Bool of bool
  | P_Array of expression array
  | P_Record of ([ `Required | `Optional ] * expression) StringMap.t
  | P_ExternalFunction of {
      parameters : string list;
      name : string;
    }
  | P_Function of {
      parameters : string list;
      body : expression;
    }
  | P_FunctionCall of {
      function_definition : expression;
      arguments : expression list;
    }
  | P_UppercaseIdentifierPathExpression of string list
  | P_UppercaseIdentifierExpression of string
  | P_LowercaseIdentifierExpression of string
  | P_TagExpression of tag
  | P_ForInExpression of {
      index : lowercase_identifier option;
      iterator : lowercase_identifier;
      reverse : bool;
      iterable : expression;
      body : expression;
    }
  | P_TemplateExpression of template_node list
  | P_BlockExpression of statement list
  | P_ConditionalExpression of {
      condition : expression;
      consequent : statement;
      alternate : statement option;
    }
  | P_UnaryExpression of Operators.Unary.t * expression
  | P_BinaryExpression of expression * Operators.Binary.t * expression

and statement = {
  statement_loc : Pinc_Diagnostics.Location.t;
  statement_desc : statement_desc;
}

and statement_desc =
  | P_CommentStatement of string
  | P_BreakStatement of int
  | P_ContinueStatement of int
  | P_UseStatement of uppercase_identifier option * expression
  | P_OptionalMutableLetStatement of lowercase_identifier * expression
  | P_OptionalLetStatement of lowercase_identifier * expression
  | P_MutableLetStatement of lowercase_identifier * expression
  | P_LetStatement of lowercase_identifier * expression
  | P_MutationStatement of lowercase_identifier * expression
  | P_ExpressionStatement of expression

and declaration = {
  declaration_loc : Pinc_Diagnostics.Location.t;
  declaration_type : declaration_type;
}

and declaration_type =
  | P_Declaration_Component of declaration_desc
  | P_Declaration_Library of declaration_desc
  | P_Declaration_Page of declaration_desc
  | P_Declaration_Store of declaration_desc

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
