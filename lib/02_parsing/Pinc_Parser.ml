module Ast = Ast

type t = Parser.t

let parse sources = sources |> Parser.parse |> Transformer.transform
