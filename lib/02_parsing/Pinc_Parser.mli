module Ast = Ast

type t

val parse : Pinc_Source.t list -> Ast.t
