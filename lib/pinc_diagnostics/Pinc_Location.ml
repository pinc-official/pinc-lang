type t =
  { loc_start : Pinc_Position.t
  ; loc_end : Pinc_Position.t
  }

let make loc_start loc_end = { loc_start; loc_end }
