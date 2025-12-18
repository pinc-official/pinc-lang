module Types = Types
module Ast = Pinc_Parser.Ast
open Types

val eval_meta :
  Pinc_Source.t list ->
  [> `Component of value StringMap.t
  | `Library of value StringMap.t
  | `Page of value StringMap.t
  | `Store of value StringMap.t
  ]
  StringMap.t

(** [eval_sources ?tag_meta_provider ~tag_data_provider ~root sources] evaluates
    definition {!root} found in {!sources}, getting its data from the
    {!tag_data_provider}.
    @raise Invalid_argument if the given root can't be evaluated (store, library). *)
val eval_sources :
  ?tag_meta_provider:Types.Type_Tag.meta_provider ->
  tag_data_provider:Types.Type_Tag.data_provider ->
  root:string ->
  Pinc_Source.t list ->
  string * (string * Types.Type_Tag.meta) list

(** [eval ?tag_meta_provider ~tag_data_provider ~root sources] evaluates definition
    {!root} found in {!sources}, getting its data from the {!tag_data_provider}.
    @raise Invalid_argument if the given root can't be evaluated (store, library). *)
val eval_declarations :
  ?tag_meta_provider:Types.Type_Tag.meta_provider ->
  tag_data_provider:Types.Type_Tag.data_provider ->
  root:string ->
  Ast.t ->
  string * (string * Types.Type_Tag.meta) list
