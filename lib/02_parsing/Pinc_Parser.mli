module Ast = Ast
module Transformer = Transformer

type t
type public_tag = Transformer.public_tag

val parse : Pinc_Source.t list -> Ast.t
val all_tags : Pinc_Source.t list -> string -> public_tag list
