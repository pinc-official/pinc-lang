type t

val make : int -> t
val to_int : t -> int
val width : t -> int
val write : Bytes.t -> int -> t -> int
val read : Bytes.t -> int -> int * t
val compare : t -> t -> int
val succ : t -> t
val incr : t ref -> unit
val pp : Format.formatter -> t -> unit

module Map : Map.S with type key := t
