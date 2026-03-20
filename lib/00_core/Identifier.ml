let is_valid_lowercase_identifier =
  String.for_all (function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false)
;;
