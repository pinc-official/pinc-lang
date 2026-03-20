module Typed_tree = Typed_tree

let typecheck ast = Infer.infer ast
