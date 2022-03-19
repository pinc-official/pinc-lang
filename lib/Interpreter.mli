module StringMap = Ast.StringMap

module rec Value : sig
  type t

  val to_string : t -> string
  val null : unit -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_float : float -> t
  val of_list : t list -> t
  val of_string_map : t StringMap.t -> t

  val make_component
    :  render:(models:(string -> t option) -> slotted_children:t list -> t)
    -> tag:string
    -> attributes:t StringMap.t
    -> children:t list
    -> t
end

and State : sig
  type t
  and environment

  and binding =
    { is_mutable : bool
    ; is_optional : bool
    ; value : Value.t
    }

  and tag_meta = { typ : tag_typ }

  and tag_typ =
    | String
    | Int
    | Float
    | Boolean
    | Array of tag_typ
    | Record of (bool * tag_typ) StringMap.t
    | Slot

  val get_output : t -> Value.t
  val get_bindings : t -> (string * binding) list
  val get_tags : t -> (string * tag_meta) list
end

val eval
  :  ?models:(string -> Value.t option)
  -> ?slotted_children:Value.t list
  -> root:StringMap.key
  -> Ast.declaration StringMap.t
  -> State.t

val from_directory
  :  ?models:(string -> Value.t option)
  -> ?slotted_children:Value.t list
  -> directory:string
  -> StringMap.key
  -> State.t

val from_file
  :  ?models:(string -> Value.t option)
  -> ?slotted_children:Value.t list
  -> filename:string
  -> StringMap.key
  -> State.t

val from_ast
  :  ?models:(string -> Value.t option)
  -> ?slotted_children:Value.t list
  -> Ast.declaration StringMap.t
  -> StringMap.key
  -> State.t
