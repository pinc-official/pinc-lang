type precedence = int

type associativity =
  | Left
  | Right

module Binary : sig
  type typ =
    | EQUAL
    | NOT_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | POW
    | MODULO
    | CONCAT
    | AND
    | OR
    | DOT_ACCESS
    | BRACKET_ACCESS
    | ARRAY_ADD
    | MERGE

  type t =
    { typ : typ
    ; closing_token : Token.token_type option
    ; precedence : precedence
    ; assoc : associativity
    }

  val make : typ -> t
end

module Unary : sig
  type typ =
    | MINUS
    | NOT

  type t =
    { typ : typ
    ; precedence : precedence
    }

  val make : typ -> t
end
