module Ast = Pinc_Frontend.Ast

type value = {
  value_loc : Pinc_Diagnostics.Location.t;
  value_desc : value_desc;
}

and value_desc =
  | Null
  | Portal of value list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Array of value array
  | Record of (int * value) StringMap.t
  | Function of function_info
  | DefinitionInfo of definition_info
  | TagInfo of tag_info
  | HtmlTemplateNode of string * value StringMap.t * value list * bool
  | ComponentTemplateNode of
      (value StringMap.t -> value) * string * value StringMap.t * value

and definition_info =
  string
  * [ `Component
    | `Site
    | `Page
    | `Store
    | `Library of (string * binding) list * (string * value) list
    ]
    option
  * [ `Negated | `NotNegated ]

and function_info = {
  parameters : string list;
  state : state;
  exec : arguments:value StringMap.t -> state:state -> unit -> value;
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

and tag_listeners = (external_tag, tag_handler) Hashtbl.t

and state = {
  mode : mode;
  binding_identifier : (bool * string) option;
  declarations : Ast.t;
  output : value;
  environment : environment;
  tag_listeners : tag_listeners;
  tag_cache : (string, value Queue.t) Hashtbl.t;
  parent_tag : string list option;
  parent_component : (string * value StringMap.t * value list) option;
  context : (string, value) Hashtbl.t;
  portals : (string, value) Hashtbl.t;
}

and environment = {
  mutable scope : (string * binding) list list;
  mutable use_scope : (string * value) list;
}

and binding = {
  is_mutable : bool;
  is_optional : bool;
  value : value;
}

and mode =
  | Portal_Collection
  | Render
