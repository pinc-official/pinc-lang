{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol  = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let check_keyword = function
  | "if"        -> IF
  | "else"      -> ELSE
  | "for"       -> FOR
  | "in"        -> IN
  | "break"     -> BREAK
  | "continue"  -> CONTINUE
  | "component" -> COMPONENT
  | "site"      -> SITE
  | "page"      -> PAGE
  | "store"     -> STORE
  | id          -> ID_LOWER id

  let trim_left str num = String.sub str num (String.length str - num)
}

let sign = '-'
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+

let int = sign? digit+
let float = sign? digit+ frac? exp?


let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let upper_id = ['A'-'Z'] ['a'-'z' 'A'-'Z']*
let lower_id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let symbol = "@" ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*


rule read =
  parse
  | whitespace    { read lexbuf }
  | newline       { next_line lexbuf; read lexbuf }
  | int           { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float         { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | '"'           { read_string (Buffer.create 17) lexbuf }
  | '('           { LEFT_PAREN }
  | ')'           { RIGHT_PAREN }
  | '{'           { LEFT_BRACE }
  | '}'           { RIGHT_BRACE }
  | '['           { LEFT_BRACK }
  | ']'           { RIGHT_BRACK }
  | ':'           { COLON }
  | ','           { COMMA }
  | '.'           { DOT }
  | '|'           { PIPE }
  | '='           { EQ }
  | "&&"          { AMPAMP }
  | "||"          { PIPEPIPE }
  | "!"           { NOT }
  | ">"           { GT }
  | "<"           { LT }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { STAR }
  | "/"           { SLASH }
  | "%"           { PERCENT }
  | symbol        { SYMBOL (trim_left (Lexing.lexeme lexbuf) 1) }
  | upper_id      { ID_UPPER (Lexing.lexeme lexbuf) }
  | lower_id      { check_keyword (Lexing.lexeme lexbuf) }
  | _             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }


and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }