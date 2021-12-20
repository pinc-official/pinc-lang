type location = Lexing.position
type t = { loc : location; declarations : Ast_Declaration.t list }
