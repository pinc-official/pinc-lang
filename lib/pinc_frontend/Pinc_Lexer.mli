module Token = Pinc_Token

type t

val make : filename:string -> string -> t
val scan : t -> Token.t

val report
  :  t
  -> start_pos:Pinc_Diagnostics.Position.t
  -> end_pos:Pinc_Diagnostics.Position.t
  -> string
  -> 'a
