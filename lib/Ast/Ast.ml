module Literal = Literal
module Operators = Operators

module SourceLocation = struct
  type t =
    { source : string
    ; pos_start : Position.t
    ; pos_end : Position.t
    }
end

type identifier = Id of string

type template_node =
  | HtmlTemplateNode of
      { tag : string
      ; attributes : attribute Iter.t
      ; children : template_node Iter.t
      }
  | ComponentTemplateNode of
      { identifier : identifier
      ; attributes : attribute Iter.t
      ; children : template_node Iter.t
      }
  | ExpressionTemplateNode of expression
  | TextTemplateNode of string

and tag =
  | TagString of
      { label : expression option
      ; placeholder : expression option
      ; inline : expression option
      ; default_value : expression option
      }
  | TagInt of
      { label : expression option
      ; placeholder : expression option
      ; default_value : expression option
      }
  | TagFloat of
      { label : expression option
      ; placeholder : expression option
      ; default_value : expression option
      }
  | TagBoolean of
      { label : expression option
      ; default_value : expression option
      }
  | TagArray of
      { label : expression option
      ; default_value : expression option
      ; elements : tag * statement option
      }

and expression =
  | IdentifierExpression of identifier
  | LiteralExpression of Literal.t
  | ArrayExpression of expression Iter.t
  | TagExpression of tag * statement option
  | ForInExpression of
      { iterator : identifier
      ; reverse : bool
      ; iterable : expression
      ; body : statement Iter.t
      }
  | ForInRangeExpression of
      { iterator : identifier
      ; reverse : bool
      ; inclusive : bool
      ; from : expression
      ; upto : expression
      ; body : statement Iter.t
      }
  | TemplateExpression of template_node Iter.t
  | BlockExpression of statement Iter.t
  | ConditionalExpression of
      { condition : expression
      ; consequent : expression
      ; alternate : expression option
      }
  | UnaryExpression of
      { operator : Operators.Unary.typ
      ; argument : expression
      }
  | BinaryExpression of
      { left : expression
      ; operator : Operators.Binary.typ
      ; right : expression
      }

and attribute =
  { key : string
  ; value : expression
  }

and statement =
  | BreakStmt
  | ContinueStmt
  | DeclarationStmt of
      { nullable : bool
      ; left : identifier
      ; right : expression
      }
  | ExpressionStmt of expression

and declaration =
  | SiteDeclaration of
      { location : Position.t
      ; identifier : identifier
      ; attributes : attribute Iter.t option
      ; body : statement Iter.t
      }
  | PageDeclaration of
      { location : Position.t
      ; identifier : identifier
      ; attributes : attribute Iter.t option
      ; body : statement Iter.t
      }
  | ComponentDeclaration of
      { location : Position.t
      ; identifier : identifier
      ; attributes : attribute Iter.t option
      ; body : statement Iter.t
      }
  | StoreDeclaration of
      { location : Position.t
      ; identifier : identifier
      ; attributes : attribute Iter.t option
      ; body : statement Iter.t
      }

and t = declaration Iter.t
