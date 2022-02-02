type precedence = int

type associativity =
  | Left
  | Right

module Binary = struct
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

  let make = function
    | POW -> { typ = POW; precedence = 8; assoc = Right }
    | TIMES -> { typ = TIMES; precedence = 7; assoc = Left }
    | DIV -> { typ = DIV; precedence = 7; assoc = Left }
    | PLUS -> { typ = PLUS; precedence = 6; assoc = Left }
    | MINUS -> { typ = MINUS; precedence = 6; assoc = Left }
    | CONCAT -> { typ = CONCAT; precedence = 5; assoc = Left }
    | EQUAL -> { typ = EQUAL; precedence = 4; assoc = Left }
    | NOT_EQUAL -> { typ = NOT_EQUAL; precedence = 4; assoc = Left }
    | GREATER -> { typ = GREATER; precedence = 4; assoc = Left }
    | GREATER_EQUAL -> { typ = GREATER_EQUAL; precedence = 4; assoc = Left }
    | LESS -> { typ = LESS; precedence = 4; assoc = Left }
    | LESS_EQUAL -> { typ = LESS_EQUAL; precedence = 4; assoc = Left }
    | AND -> { typ = AND; precedence = 2; assoc = Left }
    | OR -> { typ = OR; precedence = 1; assoc = Left }
  ;;
end

module Unary = struct
  type t =
    | NEGATIVE
    | NOT
end