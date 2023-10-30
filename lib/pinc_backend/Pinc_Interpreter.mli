open Pinc_Interpreter_Types
module Ast = Pinc_Frontend.Ast

module rec Value : sig
  val to_string : value -> string
  val null : value_loc:Pinc_Diagnostics.Location.t -> unit -> value
  val of_string : value_loc:Pinc_Diagnostics.Location.t -> string -> value
  val of_bool : value_loc:Pinc_Diagnostics.Location.t -> bool -> value
  val of_int : value_loc:Pinc_Diagnostics.Location.t -> int -> value
  val of_float : value_loc:Pinc_Diagnostics.Location.t -> float -> value
  val of_list : value_loc:Pinc_Diagnostics.Location.t -> value list -> value

  val of_string_map :
    value_loc:Pinc_Diagnostics.Location.t -> (int * value) StringMap.t -> value

  val make_component :
    render:(value StringMap.t -> value) ->
    tag:string ->
    attributes:value StringMap.t ->
    value
end

and State : sig
  val get_output : state -> value
  val get_bindings : state -> binding StringMap.t
  val get_parent_component : state -> (string * value StringMap.t * value list) option
end

val eval_meta :
  env:Eio_unix.Stdenv.base ->
  ?tag_listeners:Pinc_Interpreter_Types.tag_listeners ->
  Ast.t ->
  [> `Component of value StringMap.t
  | `Library of value StringMap.t
  | `Page of value StringMap.t
  | `Site of value StringMap.t
  | `Store of value StringMap.t
  ]
  StringMap.t

val eval :
  env:Eio_unix.Stdenv.base ->
  ?tag_listeners:Pinc_Interpreter_Types.tag_listeners ->
  root:StringMap.key ->
  Ast.t ->
  state

val from_source :
  env:Eio_unix.Stdenv.base -> ?filename:string -> source:string -> string -> state
