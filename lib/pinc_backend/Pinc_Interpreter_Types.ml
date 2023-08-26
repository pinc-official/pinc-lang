module Ast = Pinc_Frontend.Ast
module Vector = CCRAL

module rec Value : sig
  type value = {
    value_loc : Pinc_Diagnostics.Location.t;
    value_desc : value_desc;
  }

  and value_desc =
    | Null
    | Portal of value list
    | Char of Uchar.t
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Array of value Vector.t
    | Record of (int * value) StringMap.t
    | Function of function_info
    | DefinitionInfo of definition_info
    | TagInfo of tag_info
    | HtmlTemplateNode of string * value StringMap.t * value list * bool
    | ComponentTemplateNode of
        (value StringMap.t -> value) * string * value StringMap.t * value

  and definition_info =
    string
    * [ `Component | `Site | `Page | `Store | `Library of Library.t ] option
    * [ `Negated | `NotNegated ]

  and function_info = {
    parameters : string list;
    state : State.state;
    exec : arguments:value StringMap.t -> state:State.state -> unit -> value;
  }

  and external_tag =
    [ `String
    | `Int
    | `Float
    | `Boolean
    | `Array
    | `Record
    | `Slot
    | `Custom of string
    ]

  and tag_info = {
    tag : external_tag;
    key : string;
    required : bool;
    attributes : value StringMap.t;
    transformer : value -> value;
  }

  and tag_handler =
    [ `String of
      required:bool ->
      attributes:value StringMap.t ->
      key:string ->
      (value, string) Result.t
    | `Int of
      required:bool ->
      attributes:value StringMap.t ->
      key:string ->
      (value, string) Result.t
    | `Float of
      required:bool ->
      attributes:value StringMap.t ->
      key:string ->
      (value, string) Result.t
    | `Boolean of
      required:bool ->
      attributes:value StringMap.t ->
      key:string ->
      (value, string) Result.t
    | `Array of
      required:bool ->
      attributes:value StringMap.t ->
      child:tag_info ->
      key:string ->
      (value, string) Result.t
    | `Record of
      required:bool ->
      attributes:value StringMap.t ->
      children:(string * tag_info) list ->
      key:string ->
      (value, string) Result.t
    | `Slot of
      required:bool ->
      attributes:value StringMap.t ->
      key:string ->
      (value, string) Result.t
    | `Custom of
      required:bool ->
      attributes:value StringMap.t ->
      parent_value:value StringMap.t option ->
      key:string ->
      (value, string) Result.t
    ]
end =
  Value

and State : sig
  type state = {
    mode : mode;
    binding_identifier : (bool * string) option;
    declarations : Ast.t;
    output : Value.value;
    environment : environment;
    tag_listeners : tag_listeners;
    tag_cache : (string, Value.value Queue.t) Hashtbl.t;
    parent_tag : string list option;
    parent_component : (string * Value.value StringMap.t * Value.value list) option;
    context : (string, Value.value) Hashtbl.t;
    portals : (string, Value.value) Hashtbl.t;
  }

  and environment = {
    mutable scope : binding StringMap.t list;
    mutable use_scope : Library.t StringMap.t;
  }

  and binding = {
    is_mutable : bool;
    is_optional : bool;
    value : Value.value;
  }

  and tag_listeners = (Value.external_tag, Value.tag_handler) Hashtbl.t

  and mode =
    | Portal_Collection
    | Render
end =
  State

and Library : sig
  type t

  val make : bindings:State.binding StringMap.t -> includes:t StringMap.t -> t
  val get_bindings : t -> State.binding StringMap.t
  val get_binding : string -> t -> State.binding option
  val get_includes : t -> t StringMap.t
  val get_include : string -> t -> t option
end = struct
  type t = {
    bindings : State.binding StringMap.t;
    includes : t StringMap.t;
  }

  let make ~bindings ~includes = { bindings; includes }
  let get_bindings t = t.bindings
  let get_binding id t = t.bindings |> StringMap.find_opt id
  let get_includes t = t.includes
  let get_include id t = t.includes |> StringMap.find_opt id
end

include State
include Value
