type token_type =
  | COMMENT

  | IDENT_LOWER of string
  | IDENT_UPPER of string
  | INT of int
  | FLOAT of float
  | STRING of string
  | SYMBOL of string

  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | LEFT_BRACE
  | RIGHT_BRACE
  | COLON
  | COMMA
  | SEMICOLON
  | DOT
  | QUESTIONMARK
  | PIPE
  | EQUAL
  | NOT
  | GREATER
  | LESS
  | PLUS
  | MINUS
  | STAR
  | STAR_STAR
  | SLASH
  | PERCENT

  | LOGICAL_AND
  | LOGICAL_OR
  | NOT_EQUAL
  | EQUAL_EQUAL
  | GREATER_EQUAL
  | LESS_EQUAL

  | KEYWORD_TRUE
  | KEYWORD_FALSE
  | KEYWORD_IF
  | KEYWORD_ELSE
  | KEYWORD_FOR
  | KEYWORD_IN
  | KEYWORD_BREAK
  | KEYWORD_CONTINUE
  | KEYWORD_COMPONENT
  | KEYWORD_SITE
  | KEYWORD_PAGE
  | KEYWORD_STORE

  | END_OF_INPUT
[@@deriving show { with_path = false }]

type t = {
  typ: token_type;
  start_pos: Position.t;
  end_pos: Position.t;
}
[@@deriving show { with_path = false }]

let make ~start_pos ~end_pos typ = { typ; start_pos; end_pos }

let to_string = function
  | FLOAT f           -> Printf.sprintf "Token.FLOAT: ........... %f" f
  | INT i             -> Printf.sprintf "Token.INT: ............. %i" i
  | STRING s          -> Printf.sprintf "Token.STRING: .......... %s" s
  | SYMBOL s          -> Printf.sprintf "Token.SYMBOL: .......... %s" s
  | IDENT_LOWER s     -> Printf.sprintf "Token.IDENT_LOWER: ..... %s" s
  | IDENT_UPPER s     -> Printf.sprintf "Token.IDENT_UPPER: ..... %s" s
  | KEYWORD_TRUE      -> "Token.KEYWORD_TRUE"
  | KEYWORD_FALSE     -> "Token.KEYWORD_FALSE"
  | LEFT_PAREN        -> "Token.LEFT_PAREN"
  | RIGHT_PAREN       -> "Token.RIGHT_PAREN"
  | LEFT_BRACE        -> "Token.LEFT_BRACE"
  | RIGHT_BRACE       -> "Token.RIGHT_BRACE"
  | LEFT_BRACK        -> "Token.LEFT_BRACK"
  | RIGHT_BRACK       -> "Token.RIGHT_BRACK"
  | SEMICOLON         -> "Token.SEMICOLON"
  | COLON             -> "Token.COLON"
  | COMMA             -> "Token.COMMA"
  | DOT               -> "Token.DOT"
  | QUESTIONMARK      -> "Token.QUESTIONMARK"
  | PIPE              -> "Token.PIPE"
  | EQUAL             -> "Token.EQUAL"
  | NOT_EQUAL         -> "Token.NOT_EQUAL"
  | EQUAL_EQUAL       -> "Token.EQUAL_EQUAL"
  | LOGICAL_AND       -> "Token.LOGICAL_AND"
  | LOGICAL_OR        -> "Token.LOGICAL_OR"
  | NOT               -> "Token.NOT"
  | GREATER           -> "Token.GREATER"
  | GREATER_EQUAL     -> "Token.GREATER_EQUAL"
  | LESS              -> "Token.LESS"
  | LESS_EQUAL        -> "Token.LESS_EQUAL"
  | PLUS              -> "Token.PLUS"
  | MINUS             -> "Token.MINUS"
  | STAR              -> "Token.STAR"
  | STAR_STAR         -> "Token.STAR_STAR"
  | SLASH             -> "Token.SLASH"
  | PERCENT           -> "Token.PERCENT"
  | KEYWORD_IF        -> "Token.KEYWORD_IF"
  | KEYWORD_ELSE      -> "Token.KEYWORD_ELSE"
  | KEYWORD_FOR       -> "Token.KEYWORD_FOR"
  | KEYWORD_IN        -> "Token.KEYWORD_IN"
  | KEYWORD_BREAK     -> "Token.KEYWORD_BREAK"
  | KEYWORD_CONTINUE  -> "Token.KEYWORD_CONTINUE"
  | KEYWORD_COMPONENT -> "Token.KEYWORD_COMPONENT"
  | KEYWORD_SITE      -> "Token.KEYWORD_SITE"
  | KEYWORD_PAGE      -> "Token.KEYWORD_PAGE"
  | KEYWORD_STORE     -> "Token.KEYWORD_STORE"
  | COMMENT           -> "Token.COMMENT"
  | END_OF_INPUT      -> "Token.END_OF_INPUT"

let keyword_of_string = function
| "true" -> Some KEYWORD_TRUE
| "false" -> Some KEYWORD_FALSE
| "if" -> Some KEYWORD_IF
| "else" -> Some KEYWORD_ELSE
| "for" -> Some KEYWORD_FOR
| "in" -> Some KEYWORD_IN
| "break" -> Some KEYWORD_BREAK
| "continue" -> Some KEYWORD_CONTINUE
| "component" -> Some KEYWORD_COMPONENT
| "site" -> Some KEYWORD_SITE
| "page" -> Some KEYWORD_PAGE
| "store" -> Some KEYWORD_STORE
| _ -> None


let lookup_keyword str =
  match keyword_of_string str with
  | Some t -> t
  | None -> match str.[0] with
    | 'A'..'Z' -> IDENT_UPPER str
    | _ -> IDENT_LOWER str

