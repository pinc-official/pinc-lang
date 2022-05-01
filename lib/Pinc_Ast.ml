module Position = Pinc_Position
module Operators = Pinc_Operators

module SourceLocation = struct
  type t = string * Position.t * Position.t
end

type uppercase_identifier = Uppercase_Id of string

and lowercase_identifier = Lowercase_Id of string

and string_template = StringInterpolation of expression | StringText of string

and component_slot = uppercase_identifier * template_node list

and template_node =
  | HtmlTemplateNode of
      { tag: string
      ; attributes: attributes
      ; children: template_node list
      ; self_closing: bool }
  | ComponentTemplateNode of
      { identifier: uppercase_identifier
      ; attributes: attributes
      ; children: template_node list }
  | ExpressionTemplateNode of expression
  | TextTemplateNode of string

and tag =
  { tag_name: string
  ; attributes: attributes
  ; transformer: (lowercase_identifier * expression) option }

and expression =
  | String of string_template list
  | Int of int
  | Float of float
  | Bool of bool
  | Array of expression array
  | Record of (bool * expression) StringMap.t
  | Function of string list * expression
  | FunctionCall of expression * expression list
  | UppercaseIdentifierExpression of string
  | LowercaseIdentifierExpression of string
  | TagExpression of tag
  | ForInExpression of
      { index: lowercase_identifier option
      ; iterator: lowercase_identifier
      ; reverse: bool
      ; iterable: expression
      ; body: statement }
  | TemplateExpression of template_node list
  | BlockExpression of statement list
  | ConditionalExpression of
      {condition: expression; consequent: statement; alternate: statement option}
  | UnaryExpression of Operators.Unary.typ * expression
  | BinaryExpression of expression * Operators.Binary.typ * expression

and attributes = expression StringMap.t

and statement =
  | BreakStatement
  | ContinueStatement
  | UseStatement of uppercase_identifier * expression
  | OptionalMutableLetStatement of lowercase_identifier * expression
  | OptionalLetStatement of lowercase_identifier * expression
  | MutableLetStatement of lowercase_identifier * expression
  | LetStatement of lowercase_identifier * expression
  | MutationStatement of lowercase_identifier * expression
  | ExpressionStatement of expression

and declaration =
  | ComponentDeclaration of attributes option * expression
  | LibraryDeclaration of attributes option * expression
  | SiteDeclaration of attributes option * expression
  | PageDeclaration of attributes option * expression
  | StoreDeclaration of attributes option * expression

and t = declaration StringMap.t
