module Token = Pinc_Token

type t

val make : filename:string -> string -> t
val scan : t -> Token.t
