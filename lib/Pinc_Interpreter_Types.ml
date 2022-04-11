type value =
  | Null
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Array of value array
  | Record of value StringMap.t
  | Function of function_info
  | DefinitionInfo of definition_info
  | TagInfo of tag_info
  | HtmlTemplateNode of string * value StringMap.t * value list * bool
  | ComponentTemplateNode of (value StringMap.t -> value) * string * value StringMap.t

and definition_info =
  string
  * [ `Component | `Site | `Page | `Store | `Library of (string * binding) list ] option
  * [ `Negated | `NotNegated ]

and function_info =
  { parameters : string list
  ; state : state
  ; exec : arguments:value StringMap.t -> state:state -> unit -> value
  }

and tag_info = string * bool * value StringMap.t * (value -> value)

and tag_handler =
  { validate : tag_info -> value -> (value, string) Result.t
  ; transform : tag_info -> value -> value
  ; get_value : state -> string -> value option
  ; eval : self:tag_handler -> state -> tag_info -> (value, string) Result.t
  }

and state =
  { binding_identifier : (bool * string) option
  ; declarations : Pinc_Ast.declaration StringMap.t
  ; output : value
  ; environment : environment
  ; tag_listeners : tag_handler StringMap.t
  ; tag_info : bool
  ; parent_component : (string * value StringMap.t * value list) option
  ; context : (string, value) Hashtbl.t
  }

and environment = { mutable scope : (string * binding) list list }

and binding =
  { is_mutable : bool
  ; is_optional : bool
  ; value : value
  }
