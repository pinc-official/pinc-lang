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

  | KEYWORD_LET
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

  | TEMPLATE
  | HTML_OPEN_TAG of string
  | HTML_CLOSE_TAG of string
  | LESS_SLASH

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
  | FLOAT f           -> Printf.sprintf "%f" f
  | INT i             -> Printf.sprintf "%i" i
  | STRING s          -> Printf.sprintf "%S" s
  | SYMBOL s          -> Printf.sprintf "@%s" (String.capitalize_ascii s)
  | IDENT_LOWER s     -> Printf.sprintf "%s" (String.lowercase_ascii s)
  | IDENT_UPPER s     -> Printf.sprintf "%s" (String.capitalize_ascii s)
  | KEYWORD_TRUE      -> "true"
  | KEYWORD_FALSE     -> "false"
  | LEFT_PAREN        -> "("
  | RIGHT_PAREN       -> ")"
  | LEFT_BRACE        -> "{"
  | RIGHT_BRACE       -> "}"
  | LEFT_BRACK        -> "["
  | RIGHT_BRACK       -> "]"
  | SEMICOLON         -> ";"
  | COLON             -> ":"
  | COMMA             -> ","
  | DOT               -> "."
  | QUESTIONMARK      -> "?"
  | PIPE              -> "|"
  | EQUAL             -> "="
  | NOT_EQUAL         -> "!="
  | EQUAL_EQUAL       -> "=="
  | LOGICAL_AND       -> "&&"
  | LOGICAL_OR        -> "||"
  | NOT               -> "!"
  | GREATER           -> ">"
  | GREATER_EQUAL     -> ">="
  | LESS              -> "<"
  | LESS_EQUAL        -> "<="
  | PLUS              -> "+"
  | MINUS             -> "-"
  | STAR              -> "*"
  | STAR_STAR         -> "**"
  | SLASH             -> "/"
  | PERCENT           -> "%"
  | KEYWORD_LET       -> "let"
  | KEYWORD_IF        -> "if"
  | KEYWORD_ELSE      -> "else"
  | KEYWORD_FOR       -> "for"
  | KEYWORD_IN        -> "in"
  | KEYWORD_BREAK     -> "break"
  | KEYWORD_CONTINUE  -> "continue"
  | KEYWORD_COMPONENT -> "component"
  | KEYWORD_SITE      -> "site"
  | KEYWORD_PAGE      -> "page"
  | KEYWORD_STORE     -> "store"
  | COMMENT           -> "// Comment"
  | TEMPLATE          -> "@Template"
  | HTML_OPEN_TAG s   -> Printf.sprintf "<%s>" s
  | HTML_CLOSE_TAG s  -> Printf.sprintf "</%s>" s
  | LESS_SLASH        -> "</"
  | END_OF_INPUT      -> "(EOF)"

let is_keyword = function
| KEYWORD_LET
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
| KEYWORD_STORE -> true
| _             -> false

let keyword_of_string = function
| "let" -> Some KEYWORD_LET
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

