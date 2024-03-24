module Ast = Pinc_Ast
module Location = Pinc_Diagnostics.Location

type t

val make : Pinc_Core.Source.t -> t
val parse : Pinc_Core.Source.t -> Ast.t
