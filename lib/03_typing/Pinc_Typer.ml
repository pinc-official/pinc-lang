module Typed_tree = Typed_tree

let typecheck ast = Infer.infer ast
let show_tree = Typed_tree.show
