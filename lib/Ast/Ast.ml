module Literal = Literal
module Operators = Operators

module SourceLocation = struct
  type t = {
    source: string;
    pos_start: Position.t;
    pos_end: Position.t;
  }
end

type identifier = Id of string

type template_node =
  | HtmlTemplateNode of {
      tag: string;
      attributes: attribute list;
      children: template_node list;
    }
  | ComponentTemplateNode of {
      identifier: identifier;
      attributes: attribute list;
      children: template_node list;
    }
  | ExpressionTemplateNode of expression
  | TextTemplateNode of string

and expression =
  | IdentifierExpression of identifier
  | LiteralExpression of Literal.t
  | ArrayExpression of expression list
  | TagExpression of tag
  | ForInExpression of {
      iterator: identifier;
      reverse: bool;
      iterable: expression;
      body: statement list;
    }
  | ForInRangeExpression of {
      iterator: identifier;
      reverse: bool;
      inclusive: bool;
      from: expression;
      upto: expression;
      body: statement list;
    }
  | TemplateExpression of template_node list
  | BlockExpression of statement list
  | ConditionalExpression of {
      condition: expression;
      consequent: expression;
      alternate: expression option;
    }
  | UnaryExpression of {
      operator: Operators.Unary.t;
      argument: expression;
    }
  | BinaryExpression of {
      left: expression;
      operator: Operators.Binary.t;
      right: expression;
    }

and attribute = {
  key: string;
  value: expression;
}

and tag = {
  name: string;
  attributes: attribute list;
  body: statement option;
}

and statement =
  | BreakStmt
  | ContinueStmt
  | DeclarationStmt of {
      nullable: bool;
      left: identifier;
      right: expression;
    }
  | ExpressionStmt of expression

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

and t = declaration list
