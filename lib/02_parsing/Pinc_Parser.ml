module Parsetree = Parsetree
module Transformer = Transformer
module Ast = Ast

type t = Parser.t
type parsetree = Parsetree.t
type public_tag = Transformer.public_tag

let parse ?include_stdlib sources = sources |> Parser.parse ?include_stdlib

let get_ast ?include_stdlib sources =
  sources |> parse ?include_stdlib |> Transformer.transform
;;

let all_tags ?include_stdlib sources declaration =
  sources |> Parser.parse ?include_stdlib |> Transformer.all_tags declaration
;;
