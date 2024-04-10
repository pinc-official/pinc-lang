module Ast = Pinc_Parser.Ast

module rec Type_Value : sig
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
    | ComponentTemplateNode of string * value StringMap.t * value

  and definition_typ =
    | Definition_Component
    | Definition_Page
    | Definition_Store of Type_Store.t
    | Definition_Library of Type_Library.t

  and definition_info = string * definition_typ option * [ `Negated | `NotNegated ]

  and function_info = {
    parameters : string list;
    state : Type_State.state;
    exec : arguments:value StringMap.t -> state:Type_State.state -> unit -> value;
  }
end =
  Type_Value

and Type_State : sig
  type state = {
    binding_identifier : (bool * string) option;
    declarations : Ast.t;
    output : Type_Value.value;
    environment : environment;
    tag_data_provider : Type_Tag.data_provider;
    root_tag_data_provider : Type_Tag.data_provider;
    tag_path : string list;
    tag_meta : (string * Type_Tag.meta) list;
    context : Type_Value.value StringMap.t;
    mode : [ `Portal_Collection | `Portal_Render ];
  }

  and environment = {
    mutable scope : binding StringMap.t list;
    mutable use_scope : Type_Library.t StringMap.t;
  }

  and binding = {
    is_mutable : bool;
    is_optional : bool;
    value : Type_Value.value;
  }
end =
  Type_State

and Type_Library : sig
  type t

  val make : bindings:Type_State.binding StringMap.t -> includes:t StringMap.t -> t
  val get_bindings : t -> Type_State.binding StringMap.t
  val get_binding : string -> t -> Type_State.binding option
  val get_includes : t -> t StringMap.t
  val get_include : string -> t -> t option
end = struct
  type t = {
    bindings : Type_State.binding StringMap.t;
    includes : t StringMap.t;
  }

  let make ~bindings ~includes = { bindings; includes }
  let get_bindings t = t.bindings
  let get_binding id t = t.bindings |> StringMap.find_opt id
  let get_includes t = t.includes
  let get_include id t = t.includes |> StringMap.find_opt id
end

and Type_Store : sig
  type t

  val make : singleton:bool -> body:Ast.expression -> t
  val is_singleton : t -> bool
  val body : t -> Ast.expression
end = struct
  type t = {
    singleton : bool;
    body : Ast.expression;
  }

  let make ~singleton ~body = { singleton; body }
  let is_singleton t = t.singleton
  let body t = t.body
end

and Type_Tag : sig
  type kind =
    | Tag_String
    | Tag_Int
    | Tag_Float
    | Tag_Boolean
    | Tag_Array
    | Tag_Record
    | Tag_Slot of
        (tag:string -> tag_data_provider:Type_Tag.data_provider -> Type_Value.value)
    | Tag_Store of Type_Store.t
    | Tag_Custom of string

  type meta =
    [ `String of string
    | `Int of int
    | `Float of float
    | `Boolean of bool
    | `Array of meta list
    | `Record of (string * meta) list
    | `SubTagPlaceholder
    ]

  type data_provider =
    tag:kind ->
    attributes:Type_Value.value StringMap.t ->
    required:bool ->
    key:string list ->
    Type_Value.value option * meta option
end =
  Type_Tag

include Type_Value
include Type_State
