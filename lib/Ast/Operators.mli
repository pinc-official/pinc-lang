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
    | CONCAT
    | AND
    | OR

  type t = {
    typ: typ;
    precedence: precedence;
    assoc: associativity;
  }

  val make : typ -> t
end

module Unary : sig
  type typ =
    | NEGATIVE
    | NOT

  type t = {
    typ: typ;
    precedence: precedence;
  }

  val make : typ -> t
end
