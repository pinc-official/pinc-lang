module Parsetree = Parsetree
module Transformer = Transformer
module Ast = Ast

type t
type parsetree = Parsetree.t
type public_tag = Transformer.public_tag

val parse : ?include_stdlib:bool -> Pinc_Source.t list -> Parsetree.t
val get_ast : ?include_stdlib:bool -> Pinc_Source.t list -> Ast.t
val all_tags : ?include_stdlib:bool -> Pinc_Source.t list -> string -> public_tag list
