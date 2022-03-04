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
    | MODULO
    | CONCAT
    | AND
    | OR
    | DOT_ACCESS
    | BRACKET_ACCESS
    | ARRAY_ADD
    | MERGE
    | RANGE
    | INCLUSIVE_RANGE

  type t =
    { typ : typ
    ; closing_token : Token.token_type option
    ; precedence : precedence
    ; assoc : associativity
    }

  let make = function
    | DOT_ACCESS ->
      { typ = DOT_ACCESS; precedence = 110; assoc = Left; closing_token = None }
    | POW -> { typ = POW; precedence = 70; assoc = Right; closing_token = None }
    | MODULO -> { typ = MODULO; precedence = 60; assoc = Left; closing_token = None }
    | TIMES -> { typ = TIMES; precedence = 60; assoc = Left; closing_token = None }
    | DIV -> { typ = DIV; precedence = 60; assoc = Left; closing_token = None }
    | PLUS -> { typ = PLUS; precedence = 50; assoc = Left; closing_token = None }
    | MINUS -> { typ = MINUS; precedence = 50; assoc = Left; closing_token = None }
    | CONCAT -> { typ = CONCAT; precedence = 40; assoc = Left; closing_token = None }
    | EQUAL -> { typ = EQUAL; precedence = 30; assoc = Left; closing_token = None }
    | NOT_EQUAL ->
      { typ = NOT_EQUAL; precedence = 30; assoc = Left; closing_token = None }
    | GREATER -> { typ = GREATER; precedence = 30; assoc = Left; closing_token = None }
    | GREATER_EQUAL ->
      { typ = GREATER_EQUAL; precedence = 30; assoc = Left; closing_token = None }
    | LESS -> { typ = LESS; precedence = 30; assoc = Left; closing_token = None }
    | LESS_EQUAL ->
      { typ = LESS_EQUAL; precedence = 30; assoc = Left; closing_token = None }
    | AND -> { typ = AND; precedence = 20; assoc = Left; closing_token = None }
    | OR -> { typ = OR; precedence = 10; assoc = Left; closing_token = None }
    | RANGE -> { typ = RANGE; precedence = 5; assoc = Left; closing_token = None }
    | INCLUSIVE_RANGE ->
      { typ = INCLUSIVE_RANGE; precedence = 5; assoc = Left; closing_token = None }
    | ARRAY_ADD -> { typ = ARRAY_ADD; precedence = 0; assoc = Left; closing_token = None }
    | MERGE -> { typ = MERGE; precedence = 0; assoc = Left; closing_token = None }
    | BRACKET_ACCESS ->
      { typ = BRACKET_ACCESS
      ; precedence = 0
      ; assoc = Left
      ; closing_token = Some Token.RIGHT_BRACK
      }
  ;;

  let to_string = function
    | DOT_ACCESS -> "."
    | POW -> "**"
    | MODULO -> "%"
    | TIMES -> "*"
    | DIV -> "/"
    | PLUS -> "+"
    | MINUS -> "-"
    | CONCAT -> "++"
    | EQUAL -> "=="
    | NOT_EQUAL -> "!="
    | GREATER -> ">"
    | GREATER_EQUAL -> ">="
    | LESS -> "<"
    | LESS_EQUAL -> "<="
    | AND -> "&&"
    | OR -> "||"
    | ARRAY_ADD -> "<-"
    | MERGE -> "@@"
    | BRACKET_ACCESS -> "["
    | RANGE -> ".."
    | INCLUSIVE_RANGE -> "..."
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

  let to_string = function
    | MINUS -> "-..."
    | NOT -> "!..."
  ;;
end
