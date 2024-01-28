module Ast = Pinc_Frontend.Ast

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
    | Array of value Array.t
    | Record of value StringMap.t
    | Function of function_info
    | DefinitionInfo of definition_info
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
end =
  Value

and State : sig
  type state = {
    binding_identifier : (bool * string) option;
    declarations : Ast.t;
    output : Value.value;
    environment : environment;
    slot_environment : Value.value list;
    tag_environment : Value.value StringMap.t;
    tag_cache : (string, Value.value Queue.t) Hashtbl.t;
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
