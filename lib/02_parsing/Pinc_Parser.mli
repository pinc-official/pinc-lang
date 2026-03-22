module Ast = Ast
module Transformer = Transformer
module DependencyGraph = DependencyGraph

type t
type public_tag = Transformer.public_tag

val parse : ?include_stdlib:bool -> Pinc_Source.t list -> Ast.t
val all_tags : Pinc_Source.t list -> string -> public_tag list
