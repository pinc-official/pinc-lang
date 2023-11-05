module Client = Pinc_Rpc_Client
module Definitions = Definitions

module Value : sig
  type t

  val null : t
  val string : string -> t
  val float : float -> t
  val int : int -> t
  val bool : bool -> t
  val record : (string * t) list -> t
  val list : t list -> t
end

val make_string_request :
  ?required:bool ->
  ?attributes:(string * Value.t) list ->
  key:string ->
  unit ->
  string * (string -> Definitions.string_response)

val make_int_request :
  ?required:bool ->
  ?attributes:(string * Value.t) list ->
  key:string ->
  unit ->
  string * (string -> Definitions.int_response)

val make_float_request :
  ?required:bool ->
  ?attributes:(string * Value.t) list ->
  key:string ->
  unit ->
  string * (string -> Definitions.float_response)

val make_bool_request :
  ?required:bool ->
  ?attributes:(string * Value.t) list ->
  key:string ->
  unit ->
  string * (string -> Definitions.bool_response)
