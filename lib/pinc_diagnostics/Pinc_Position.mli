type t = {
  source : Pinc_Core.Source.t;
  line : int;
  beginning_of_line : int;
  column : int;
}

val make : source:Pinc_Core.Source.t -> line:int -> column:int -> t
