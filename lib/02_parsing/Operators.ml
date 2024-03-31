type precedence = int

type associativity =
  | Assoc_Left
  | Assoc_Right

module Binary = struct
  type t =
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
    | FUNCTION_CALL
    | PIPE
    | ARRAY_ADD
    | MERGE
    | RANGE
    | INCLUSIVE_RANGE

  let get_precedence = function
    | DOT_ACCESS -> 110
    | FUNCTION_CALL -> 100
    | POW -> 70
    | MODULO | TIMES | DIV -> 60
    | PLUS | MINUS -> 50
    | CONCAT -> 40
    | EQUAL | NOT_EQUAL | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL -> 30
    | AND -> 20
    | OR -> 10
    | RANGE | INCLUSIVE_RANGE -> 5
    | ARRAY_ADD | MERGE | BRACKET_ACCESS | PIPE -> 0
  ;;

  let get_associativity = function
    | POW -> Assoc_Right
    | _ -> Assoc_Left
  ;;

  let get_closing_token = function
    | FUNCTION_CALL -> Some Token.RIGHT_PAREN
    | BRACKET_ACCESS -> Some Token.RIGHT_BRACK
    | _ -> None
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
    | FUNCTION_CALL -> "("
    | RANGE -> ".."
    | INCLUSIVE_RANGE -> "..."
    | PIPE -> "|>"
  ;;
end

module Unary = struct
  type t =
    | MINUS
    | NOT

  let get_precedence = function
    | MINUS -> 100
    | NOT -> 100
  ;;

  let to_string = function
    | MINUS -> "-..."
    | NOT -> "!..."
  ;;
end
