type t =
  { filename : string
  ; line : int
  ; column : int
  }

val make : filename:string -> line:int -> column:int -> t
