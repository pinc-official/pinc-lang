module Env : sig
  type t = { current_identifier : ([ `Optional | `Required ] * string) option }
end

val transform : Parsetree.t -> Ast.t
