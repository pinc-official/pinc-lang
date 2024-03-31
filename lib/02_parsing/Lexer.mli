type t

val make : Pinc_Source.t -> t
val scan : t -> Token.t
