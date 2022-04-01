module Ast = Pinc_Ast
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
  module Tag : sig
    type t =
      { name : string
      ; key : string
      ; is_optional : bool
      ; value : Value.t
      }
  end

  type t
  and environment

  and binding =
    { is_mutable : bool
    ; is_optional : bool
    ; value : Value.t
    }

  val get_output : t -> Value.t
  val get_bindings : t -> (string * binding) list
end

val eval
  :  ?tag_listeners:(State.Tag.t -> unit) StringMap.t
  -> ?models:(string -> Value.t option)
  -> ?slotted_children:Value.t list
  -> ?context:Value.t StringMap.t
  -> root:StringMap.key
  -> Ast.declaration StringMap.t
  -> State.t

val from_source
  :  ?models:(string -> Value.t option)
  -> ?slotted_children:Value.t list
  -> ?filename:string
  -> source:string
  -> string
  -> State.t
