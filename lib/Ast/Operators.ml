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
    | CONCAT
    | AND
    | OR

  let get_prec_and_assoc = function
    | POW -> 8, `right
    | TIMES -> 7, `left
    | DIV -> 7, `left
    | PLUS -> 6, `left
    | MINUS -> 6, `left
    | CONCAT -> 5, `left
    | EQUAL -> 4, `left
    | NOT_EQUAL -> 4, `left
    | GREATER -> 4, `left
    | GREATER_EQUAL -> 4, `left
    | LESS -> 4, `left
    | LESS_EQUAL -> 4, `left
    | AND -> 2, `left
    | OR -> 1, `left
  ;;
end

module Unary = struct
  type t =
    | NEGATIVE
    | NOT

  let get_prec_and_assoc = function
    | NEGATIVE -> 300, `right
    | NOT -> 300, `right
  ;;
end