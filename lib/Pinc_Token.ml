module Position = Pinc_Position

type token_type =
  | COMMENT
  | IDENT_LOWER of string
  | IDENT_UPPER of string
  | INT of int
  | FLOAT of float
  | STRING of string
  | TAG of string
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_PIPE_BRACE
  | RIGHT_PIPE_BRACE
  | DOUBLE_QUOTE
  | COLON
  | DOUBLE_COLON
  | COMMA
  | SEMICOLON
  | DOT
  | DOTDOT
  | DOTDOTDOT
  | ARROW
  | ARROW_LEFT
  | PIPE
  | ATAT
  | QUESTIONMARK
  | EQUAL
  | COLON_EQUAL
  | NOT
  | GREATER
  | LESS
  | PLUS
  | UNARY_MINUS
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
  | PLUSPLUS
  | KEYWORD_FN
  | KEYWORD_LET
  | KEYWORD_MUTABLE
  | KEYWORD_TRUE
  | KEYWORD_FALSE
  | KEYWORD_IF
  | KEYWORD_ELSE
  | KEYWORD_FOR
  | KEYWORD_IN
  | KEYWORD_REVERSE
  | KEYWORD_BREAK
  | KEYWORD_CONTINUE
  | KEYWORD_COMPONENT
  | KEYWORD_LIBRARY
  | KEYWORD_SITE
  | KEYWORD_PAGE
  | KEYWORD_STORE
  | HTML_OPEN_FRAGMENT
  | HTML_CLOSE_FRAGMENT
  | HTML_OPEN_TAG of string
  | HTML_CLOSE_TAG of string
  | COMPONENT_OPEN_TAG of string
  | COMPONENT_CLOSE_TAG of string
  | HTML_OR_COMPONENT_TAG_SELF_CLOSING
  | HTML_OR_COMPONENT_TAG_END
  | END_OF_INPUT

type t =
  { typ : token_type
  ; start_pos : Position.t
  ; end_pos : Position.t
  }

let make ~start_pos ~end_pos typ = { typ; start_pos; end_pos }

let to_string = function
  | FLOAT f -> string_of_float f
  | INT i -> string_of_int i
  | STRING s -> "\"" ^ s ^ "\""
  | IDENT_LOWER s -> String.lowercase_ascii s
  | IDENT_UPPER s -> String.capitalize_ascii s
  | TAG s -> "#" ^ String.capitalize_ascii s
  | KEYWORD_TRUE -> "true"
  | KEYWORD_FALSE -> "false"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | LEFT_PIPE_BRACE -> "{|"
  | RIGHT_PIPE_BRACE -> "|}"
  | DOUBLE_QUOTE -> "\""
  | LEFT_BRACK -> "["
  | RIGHT_BRACK -> "]"
  | SEMICOLON -> ";"
  | DOUBLE_COLON -> "::"
  | COLON -> ":"
  | COMMA -> ","
  | DOT -> "."
  | DOTDOT -> ".."
  | DOTDOTDOT -> "..."
  | QUESTIONMARK -> "?"
  | ARROW -> "->"
  | ARROW_LEFT -> "<-"
  | PIPE -> "|>"
  | ATAT -> "@@"
  | EQUAL -> "="
  | COLON_EQUAL -> ":="
  | NOT_EQUAL -> "!="
  | EQUAL_EQUAL -> "=="
  | LOGICAL_AND -> "&&"
  | LOGICAL_OR -> "||"
  | NOT -> "!"
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | PLUSPLUS -> "++"
  | PLUS -> "+"
  | MINUS -> "-"
  | UNARY_MINUS -> "-"
  | STAR -> "*"
  | STAR_STAR -> "**"
  | SLASH -> "/"
  | PERCENT -> "%"
  | KEYWORD_FN -> "fn"
  | KEYWORD_LET -> "let"
  | KEYWORD_MUTABLE -> "mutable"
  | KEYWORD_IF -> "if"
  | KEYWORD_ELSE -> "else"
  | KEYWORD_FOR -> "for"
  | KEYWORD_IN -> "in"
  | KEYWORD_REVERSE -> "reverse"
  | KEYWORD_BREAK -> "break"
  | KEYWORD_CONTINUE -> "continue"
  | KEYWORD_COMPONENT -> "component"
  | KEYWORD_LIBRARY -> "library"
  | KEYWORD_SITE -> "site"
  | KEYWORD_PAGE -> "page"
  | KEYWORD_STORE -> "store"
  | COMMENT -> "// Comment"
  | HTML_OPEN_FRAGMENT -> "<>"
  | HTML_CLOSE_FRAGMENT -> "</>"
  | HTML_OPEN_TAG s -> "<" ^ s
  | HTML_CLOSE_TAG s -> "</" ^ s
  | COMPONENT_OPEN_TAG s -> "<" ^ s
  | COMPONENT_CLOSE_TAG s -> "</" ^ s
  | HTML_OR_COMPONENT_TAG_SELF_CLOSING -> "/>"
  | HTML_OR_COMPONENT_TAG_END -> "> (TAG END)"
  | END_OF_INPUT -> "(EOF)"
;;

let is_keyword = function
  | KEYWORD_FN
  | KEYWORD_LET
  | KEYWORD_MUTABLE
  | KEYWORD_TRUE
  | KEYWORD_FALSE
  | KEYWORD_IF
  | KEYWORD_ELSE
  | KEYWORD_FOR
  | KEYWORD_IN
  | KEYWORD_BREAK
  | KEYWORD_CONTINUE
  | KEYWORD_COMPONENT
  | KEYWORD_LIBRARY
  | KEYWORD_SITE
  | KEYWORD_PAGE
  | KEYWORD_STORE -> true
  | COMMENT
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_PIPE_BRACE
  | RIGHT_PIPE_BRACE
  | DOUBLE_QUOTE
  | COLON
  | DOUBLE_COLON
  | COMMA
  | SEMICOLON
  | DOT
  | DOTDOT
  | DOTDOTDOT
  | ARROW
  | ARROW_LEFT
  | PIPE
  | ATAT
  | QUESTIONMARK
  | EQUAL
  | COLON_EQUAL
  | NOT
  | GREATER
  | LESS
  | PLUS
  | UNARY_MINUS
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
  | PLUSPLUS
  | KEYWORD_REVERSE
  | HTML_OPEN_FRAGMENT
  | HTML_CLOSE_FRAGMENT
  | HTML_OR_COMPONENT_TAG_END
  | HTML_OR_COMPONENT_TAG_SELF_CLOSING
  | END_OF_INPUT
  | IDENT_LOWER _
  | IDENT_UPPER _
  | INT _
  | FLOAT _
  | STRING _
  | TAG _
  | HTML_OPEN_TAG _
  | HTML_CLOSE_TAG _
  | COMPONENT_OPEN_TAG _
  | COMPONENT_CLOSE_TAG _ -> false
;;

let keyword_of_string = function
  | "fn" -> Some KEYWORD_FN
  | "let" -> Some KEYWORD_LET
  | "mutable" -> Some KEYWORD_MUTABLE
  | "true" -> Some KEYWORD_TRUE
  | "false" -> Some KEYWORD_FALSE
  | "if" -> Some KEYWORD_IF
  | "else" -> Some KEYWORD_ELSE
  | "for" -> Some KEYWORD_FOR
  | "in" -> Some KEYWORD_IN
  | "reverse" -> Some KEYWORD_REVERSE
  | "break" -> Some KEYWORD_BREAK
  | "continue" -> Some KEYWORD_CONTINUE
  | "component" -> Some KEYWORD_COMPONENT
  | "library" -> Some KEYWORD_LIBRARY
  | "site" -> Some KEYWORD_SITE
  | "page" -> Some KEYWORD_PAGE
  | "store" -> Some KEYWORD_STORE
  | _ -> None
;;

let lookup_keyword str =
  match keyword_of_string str with
  | Some t -> t
  | None ->
    (match str.[0] with
    | 'A' .. 'Z' -> IDENT_UPPER str
    | _ -> IDENT_LOWER str)
;;
