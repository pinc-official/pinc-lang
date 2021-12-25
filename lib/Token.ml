type token_type =
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

type t = { typ : token_type; start_pos : Position.t; end_pos : Position.t }

let make ~start_pos ~end_pos typ = { typ; start_pos; end_pos }

let to_string = function
  | FLOAT f -> Printf.sprintf "Float: %f" f
  | INT i -> Printf.sprintf "Int: %i" i
  | STRING s -> Printf.sprintf "String: \"%s\"" s
  | KEYWORD_TRUE -> "true"
  | KEYWORD_FALSE -> "false"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | LEFT_BRACK -> "["
  | RIGHT_BRACK -> "]"
  | SEMICOLON -> ";"
  | COLON -> ":"
  | COMMA -> ","
  | DOT -> "."
  | PIPE -> "|"
  | EQUAL -> "="
  | NOT_EQUAL -> "!="
  | EQUAL_EQUAL -> "=="
  | LOGICAL_AND -> "&&"
  | LOGICAL_OR -> "||"
  | NOT -> "!"
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | STAR_STAR -> "**"
  | SLASH -> "/"
  | PERCENT -> "%"
  | KEYWORD_IF -> "if"
  | KEYWORD_ELSE -> "else"
  | KEYWORD_FOR -> "for"
  | KEYWORD_IN -> "in"
  | KEYWORD_BREAK -> "break"
  | KEYWORD_CONTINUE -> "continue"
  | KEYWORD_COMPONENT -> "component"
  | KEYWORD_SITE -> "site"
  | KEYWORD_PAGE -> "page"
  | KEYWORD_STORE -> "store"
  | SYMBOL s -> Printf.sprintf "Symbol: %s" s
  | IDENT_LOWER s -> Printf.sprintf "Lower Ident: %s" s
  | IDENT_UPPER s -> Printf.sprintf "Upper Ident: %s" s
  | END_OF_INPUT -> "(THE_END)"

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

