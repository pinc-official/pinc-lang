module StringMap = Map.Make (String)
module Position = Pinc_Position
module Operators = Pinc_Operators

module SourceLocation = struct
  type t = string * Position.t * Position.t
end

type uppercase_identifier = Uppercase_Id of string
and lowercase_identifier = Lowercase_Id of string

and string_template =
  | StringInterpolation of expression
  | StringText of string

and component_slot = uppercase_identifier * template_node list

and template_node =
  | HtmlTemplateNode of
      { tag : string
      ; attributes : attributes
      ; children : template_node list
      ; self_closing : bool
      }
  | ComponentTemplateNode of
      { identifier : uppercase_identifier
      ; attributes : attributes
      ; children : template_node list
      }
  | ExpressionTemplateNode of expression
  | TextTemplateNode of string

and tag =
  { tag_name : string
  ; attributes : attributes
  ; transformer : (lowercase_identifier * expression) option
  ; exec : bool
  }

and block = expression list

and expression =
  | String of string_template list
  | Int of int
  | Float of float
  | Bool of bool
  | Array of expression array
  | Record of (bool * expression) StringMap.t
  | Function of string list * block
  | FunctionCall of expression * expression list
  | UppercaseIdentifierExpression of uppercase_identifier
  | LowercaseIdentifierExpression of lowercase_identifier
  | OptionalLetExpression of bool * lowercase_identifier * expression
  | LetExpression of bool * lowercase_identifier * expression
  | MutationExpression of lowercase_identifier * expression
  | TagExpression of tag
  | BreakExpression of expression
  | ContinueExpression of expression
  | ForInExpression of
      { index : lowercase_identifier option
      ; iterator : lowercase_identifier
      ; reverse : bool
      ; iterable : expression
      ; body : block
      }
  | TemplateExpression of template_node list
  | BlockExpression of block
  | ConditionalExpression of
      { condition : expression
      ; consequent : block
      ; alternate : block option
      }
  | UnaryExpression of Operators.Unary.typ * expression
  | BinaryExpression of expression * Operators.Binary.typ * expression

and attributes = expression StringMap.t

and declaration =
  | ComponentDeclaration of attributes option * block
  | SiteDeclaration of attributes option * block
  | PageDeclaration of attributes option * block
  | StoreDeclaration of attributes option * block

and t = declaration StringMap.t
