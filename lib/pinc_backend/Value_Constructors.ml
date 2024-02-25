open Types.Type_Value

let null ?(value_loc = Pinc_Diagnostics.Location.none) () =
  { value_loc; value_desc = Null }
;;

let of_char ?(value_loc = Pinc_Diagnostics.Location.none) c =
  { value_loc; value_desc = Char c }
;;

let of_string ?(value_loc = Pinc_Diagnostics.Location.none) s =
  { value_loc; value_desc = String s }
;;

let of_bool ?(value_loc = Pinc_Diagnostics.Location.none) b =
  { value_loc; value_desc = Bool b }
;;

let of_int ?(value_loc = Pinc_Diagnostics.Location.none) i =
  { value_loc; value_desc = Int i }
;;

let of_float ?(value_loc = Pinc_Diagnostics.Location.none) f =
  { value_loc; value_desc = Float f }
;;

let of_array ?(value_loc = Pinc_Diagnostics.Location.none) l =
  { value_loc; value_desc = Array l }
;;

let of_list ?(value_loc = Pinc_Diagnostics.Location.none) l =
  { value_loc; value_desc = Array (Array.of_list l) }
;;

let of_string_map ?(value_loc = Pinc_Diagnostics.Location.none) m =
  { value_loc; value_desc = Record m }
;;
