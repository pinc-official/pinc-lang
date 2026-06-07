module Diagnostics = Pinc_Diagnostics
module Types = Types
module Ast = Pinc_Types.Ast
open Types

val eval_meta :
  Ast.t ->
  [> `Component of Diagnostics.Location.t * value StringMap.t
  | `Library of Diagnostics.Location.t * value StringMap.t
  | `Page of Diagnostics.Location.t * value StringMap.t
  | `Store of Diagnostics.Location.t * value StringMap.t
  ]
  StringMap.t

(** [eval ?tag_meta_provider ~tag_data_provider ~root sources] evaluates definition
    {!root} found in {!sources}, getting its data from the {!tag_data_provider}.
    @raise Invalid_argument if the given root can't be evaluated (store, library). *)
val eval_declarations :
  ?tag_meta_provider:Types.Type_Tag.meta_provider ->
  tag_data_provider:Types.Type_Tag.data_provider ->
  root:string ->
  Ast.t ->
  string * (string * Types.Type_Tag.meta) list
