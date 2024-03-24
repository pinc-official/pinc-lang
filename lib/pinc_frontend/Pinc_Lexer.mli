module Token = Pinc_Token

type t

val make : Pinc_Core.Source.t -> t
val scan : t -> Token.t
