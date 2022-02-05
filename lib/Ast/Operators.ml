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

  type t =
    { typ : typ
    ; precedence : precedence
    ; assoc : associativity
    }

  let make = function
    | POW -> { typ = POW; precedence = 70; assoc = Right }
    | TIMES -> { typ = TIMES; precedence = 60; assoc = Left }
    | DIV -> { typ = DIV; precedence = 60; assoc = Left }
    | PLUS -> { typ = PLUS; precedence = 50; assoc = Left }
    | MINUS -> { typ = MINUS; precedence = 50; assoc = Left }
    | CONCAT -> { typ = CONCAT; precedence = 40; assoc = Left }
    | EQUAL -> { typ = EQUAL; precedence = 30; assoc = Left }
    | NOT_EQUAL -> { typ = NOT_EQUAL; precedence = 30; assoc = Left }
    | GREATER -> { typ = GREATER; precedence = 30; assoc = Left }
    | GREATER_EQUAL -> { typ = GREATER_EQUAL; precedence = 30; assoc = Left }
    | LESS -> { typ = LESS; precedence = 30; assoc = Left }
    | LESS_EQUAL -> { typ = LESS_EQUAL; precedence = 30; assoc = Left }
    | AND -> { typ = AND; precedence = 20; assoc = Left }
    | OR -> { typ = OR; precedence = 10; assoc = Left }
  ;;
end

module Unary = struct
  type typ =
    | MINUS
    | NOT

  type t =
    { typ : typ
    ; precedence : precedence
    }

  let make = function
    | MINUS -> { typ = MINUS; precedence = 100 }
    | NOT -> { typ = NOT; precedence = 100 }
  ;;
end
