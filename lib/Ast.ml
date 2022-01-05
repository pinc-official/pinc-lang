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

  type unary =
  | NEGATIVE | POSITIVE
  | NOT
  [@@deriving show { with_path = false }]
end

type literal =
  | StringLiteral of string
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
[@@deriving show { with_path = false }]

and expression = 
  | IdentifierExpression of string
  | LiteralExpression of literal
  | ArrayExpression of expression list
  | SymbolsExpression of symbol list
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
    right: expression;
    operator: Operator.binary;
  }
  | LogicalExpression of {
    left: expression;
    right: expression;
    operator: Operator.logical
  }
  | AssignmentExpression of {
    nullable: bool;
    left: expression;
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
  | ExpressionStmt of expression
  | BlockStmt of statement list
  | ForInStmt of {
    left: expression;
    right: expression;
    body: statement list
  }
[@@deriving show { with_path = false }]

and declaration =
  | Site of {
    identifier: expression;
    attributes: attribute list option;
    body: statement;
  }
  | Page of {
    identifier: expression;
    attributes: attribute list option;
    body: statement;
  }
  | Component of {
    identifier: expression;
    attributes: attribute list option;
    body: statement;
  }
  | Store of {
    identifier: expression;
    attributes: attribute list option;
    body: statement;
  }
[@@deriving show { with_path = false }]

and file = {
  location: Position.t;
  declarations: declaration list;
}
[@@deriving show { with_path = false }]


and t = file list
[@@deriving show { with_path = false }]
