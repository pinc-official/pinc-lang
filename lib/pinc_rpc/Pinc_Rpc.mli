module Value : sig
  type t

  val string : string -> t
  val float : float -> t
  val int : int -> t
  val bool : bool -> t
  val record : (string * t) list -> t
  val list : t list -> t
end

val make_string_request :
  net:[> `Network | `Platform of [ `Generic | `Unix ] ] Eio.Resource.t ->
  ?required:bool ->
  ?attributes:(string * Value.t) list ->
  key:string ->
  string * string ->
  string

val make_int_request :
  ?required:bool -> ?attributes:(string * Value.t) list -> key:string -> unit -> string

val make_float_request :
  ?required:bool -> ?attributes:(string * Value.t) list -> key:string -> unit -> string

val make_bool_request :
  ?required:bool -> ?attributes:(string * Value.t) list -> key:string -> unit -> string
