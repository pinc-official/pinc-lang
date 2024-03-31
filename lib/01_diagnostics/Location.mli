module Position : sig
  type t

  val make : source:Pinc_Source.t -> line:int -> column:int -> t
  val get_line : t -> int
  val get_column : t -> int
  val get_source : t -> Pinc_Source.t
end

type t

val merge : s:t -> e:t -> unit -> t
val make : ?e:Position.t -> s:Position.t -> unit -> t
val none : t
val get_source : t -> Pinc_Source.t
val get_start : t -> Position.t
val get_end : t -> Position.t
val pp : Format.formatter -> t -> unit
