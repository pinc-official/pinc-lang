open Pinc_Interpreter_Types
module Ast = Pinc_Ast

module rec Value : sig
  val to_string : value -> string
  val null : unit -> value
  val of_string : string -> value
  val of_bool : bool -> value
  val of_int : int -> value
  val of_float : float -> value
  val of_list : value list -> value
  val of_string_map : value StringMap.t -> value

  val make_component
    :  render:(value StringMap.t -> value)
    -> tag:string
    -> attributes:value StringMap.t
    -> value
end

and State : sig
  val get_output : state -> value
  val get_bindings : state -> (string * binding) list
  val get_parent_component : state -> (string * value StringMap.t * value list) option
end

val eval
  :  ?tag_listeners:Pinc_Interpreter_Types.tag_handler StringMap.t
  -> root:StringMap.key
  -> Ast.declaration StringMap.t
  -> state

val from_source : ?filename:string -> source:string -> string -> state
