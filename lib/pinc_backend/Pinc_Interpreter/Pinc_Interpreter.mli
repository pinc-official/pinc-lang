module Types = Types
module Ast = Pinc_Frontend.Ast
open Types

val noop_data_provider : Type_Tag.data_provider

val eval_meta :
  Ast.t ->
  [> `Component of value StringMap.t
  | `Library of value StringMap.t
  | `Page of value StringMap.t
  | `Store of value StringMap.t
  ]
  StringMap.t

val eval :
  tag_data_provider:Types.Type_Tag.data_provider -> root:StringMap.key -> Ast.t -> string
