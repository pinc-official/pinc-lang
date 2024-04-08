module Ast = Ast

type t

val make : Pinc_Source.t -> t
val parse : Pinc_Source.t -> (string * Ast.declaration) list
