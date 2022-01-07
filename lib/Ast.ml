module SourceLocation = struct
  type t = {
    source: string;
    pos_start: Position.t;
    pos_end: Position.t;
  }
  [@@deriving show { with_path = false }]
end

module Operator = struct
  type binary = 
  | EQUAL | NOT_EQUAL
  | GREATER | GREATER_EQUAL
  | LESS | LESS_EQUAL
  | PLUS | MINUS | TIMES | DIV
  | IN
  [@@deriving show { with_path = false }]

  type logical = AND | OR
  [@@deriving show { with_path = false }]

  type unary = NEGATIVE | NOT
  [@@deriving show { with_path = false }]
end

type identifier = | Id of string
[@@deriving show { with_path = false }]

type literal =
  | StringLiteral of string
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
[@@deriving show { with_path = false }]

and template_node =
  | HtmlTemplateNode of {
    tag: string;
    self_closing: bool;
    attributes: attribute list;
    children: template_node list;
  }
  | ComponentTemplateNode of {
    identifier: identifier;
    self_closing: bool;
    attributes: attribute list;
    children: template_node list;
  }
  | ExpressionTemplateNode of expression
  | TextTemplateNode of string

and expression =
  | IdentifierExpression of identifier
  | LiteralExpression of literal
  | ArrayExpression of expression list
  | SymbolExpression of symbol

  | ForInExpression of {
    left: identifier;
    right: expression;
    body: statement list
  }

  | TemplateExpression of template_node list

  | BlockExpression of statement list

  | ConditionalExpression of {
    condition: expression;
    consequent: expression;
    alternate: expression option;
  }
  | UnaryExpression of {
    operator: Operator.unary;
    argument: expression;
  }
  | BinaryExpression of {
    left: expression;
    operator: Operator.binary;
    right: expression;
  }
  | LogicalExpression of {
    left: expression;
    operator: Operator.logical;
    right: expression;
  }
[@@deriving show { with_path = false }]

and attribute = {
  key: string;
  value: expression
}
[@@deriving show { with_path = false }]

and symbol = {
  name: string;
  attributes: attribute list;
  body: statement option;
}
[@@deriving show { with_path = false }]

and statement = 
  | BreakStmt
  | ContinueStmt
  | DeclarationStmt of {
    nullable: bool;
    left: identifier;
    right: expression;
  }
  | ExpressionStmt of expression
[@@deriving show { with_path = false }]

and declaration =
  | SiteDeclaration of {
    location: Position.t;
    identifier: identifier;
    attributes: attribute list option;
    body: statement list;
  }
  | PageDeclaration of {
    location: Position.t;
    identifier: identifier;
    attributes: attribute list option;
    body: statement list;
  }
  | ComponentDeclaration of {
    location: Position.t;
    identifier: identifier;
    attributes: attribute list option;
    body: statement list;
  }
  | StoreDeclaration of {
    location: Position.t;
    identifier: identifier;
    attributes: attribute list option;
    body: statement list;
  }
[@@deriving show { with_path = false }]

and t = declaration list
[@@deriving show { with_path = false }]
