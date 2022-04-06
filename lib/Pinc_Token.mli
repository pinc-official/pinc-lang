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

val make : start_pos:Position.t -> end_pos:Position.t -> token_type -> t
val to_string : token_type -> string
val is_keyword : token_type -> bool
val lookup_keyword : string -> token_type