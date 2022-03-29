module Ast = Pinc_Ast

type t

val make : filename:string -> string -> t
val scan : t -> Ast.declaration Ast.StringMap.t
val parse : filename:string -> string -> Ast.declaration Ast.StringMap.t
