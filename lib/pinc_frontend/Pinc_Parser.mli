module Ast = Pinc_Ast

type t

val make : filename:string -> string -> t
val parse : filename:string -> string -> Ast.declaration StringMap.t
