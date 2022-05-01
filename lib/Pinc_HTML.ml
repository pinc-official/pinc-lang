let void_elements =
  [| "area"
   ; "base"
   ; "br"
   ; "col"
   ; "embed"
   ; "hr"
   ; "img"
   ; "input"
   ; "link"
   ; "meta"
   ; "param"
   ; "source"
   ; "track"
   ; "wbr" |]

let is_void_el s = Array.mem s void_elements
