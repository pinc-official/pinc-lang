module Ast = Ast
module Transformer = Transformer

type t = Parser.t
type public_tag = Transformer.public_tag

let parse sources = sources |> Parser.parse |> Transformer.transform

let all_tags sources declaration =
  sources |> Parser.parse |> Transformer.all_tags declaration
;;
