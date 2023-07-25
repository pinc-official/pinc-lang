module Ast = Pinc_Ast
module Location = Pinc_Diagnostics.Location

type t

val make : filename:string -> string -> t
val parse : filename:string -> string -> Ast.t
