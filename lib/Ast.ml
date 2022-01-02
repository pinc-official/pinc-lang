module rec SourceLocation: sig
  type t = {
    source: string;
    pos_start: Position.t;
    pos_end: Position.t;
  }
end = SourceLocation

and Operator: sig
  type binary = 
  | EQUAL | NOT_EQUAL
  | GREATER | GREATER_EQUAL
  | LESS | LESS_EQUAL
  | PLUS | MINUS | TIMES | DIV
  | IN

  type logical = AND | OR

  type unary =
  | NEGATIVE | POSITIVE
  | NOT
end = Operator

and Identifier: sig
  type t = string
end = Identifier

and Literal: sig
  type t = 
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
end = Literal

and Symbol: sig
  type t = {
    identifier: string;
    attributes: Attribute.t list;
    body: Statement.t option;
  }
end = Symbol

and Attribute: sig
  type t = {
    key: Identifier.t;
    value: Expression.t
  }
end = Attribute

and Expression: sig
  type t = 
    | Literal of Literal.t
    | Array of Expression.t list
    | Symbols of Symbol.t list
    | Conditional of {
      condition: Expression.t;
      consequent: Expression.t;
      alternate: Expression.t option;
    }
    | Unary of {
      operator: Operator.unary;
      argument: Expression.t;
    }
    | Binary of {
      left: Expression.t;
      right: Expression.t;
      operator: Operator.binary;
    }
    | Logical of {
      left: Expression.t;
      right: Expression.t;
      operator: Operator.logical
    }
    | Assignment of {
      left: Identifier.t;
      right: Expression.t;
    }
end = Expression

and Statement: sig
  type t = 
    | Break
    | Continue
    | Expression of Expression.t
    | Block of Statement.t list
    | ForIn of {
      left: Identifier.t;
      right: Expression.t;
      body: Statement.t list
    }
end = Statement


and Declaration: sig
  type content = {
    identifier: Identifier.t;
    attributes: Attribute.t list option;
    body: Statement.t;
  }

  type t =
    | Site of content
    | Page of content
    | Component of content
    | Store of content
end = Declaration

and File: sig
  type t = {
    location: Position.t;
    declarations: Declaration.t list;
  }
end = File

type t = File.t list
