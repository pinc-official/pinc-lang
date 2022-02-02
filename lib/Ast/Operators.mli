module Binary : sig
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

  val get_prec_and_assoc : t -> int * [> `left | `right ]
end

module Unary : sig
  type t =
    | NEGATIVE
    | NOT

  val get_prec_and_assoc : t -> int * [> `right ]
end
