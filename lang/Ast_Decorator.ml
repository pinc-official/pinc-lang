type ident = string
type t = { ident : ident; attrs : Ast_Attr.t list }

let make ident attrs = { ident; attrs }
