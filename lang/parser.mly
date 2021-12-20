%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACK RIGHT_BRACK
%token COLON COMMA DOT
%token AT
%token PIPE
%token EQ
%token IF ELSE FOR IN BREAK CONTINUE
%token AMPAMP PIPEPIPE NOT
%token GT LT

%token PLUS MINUS STAR SLASH PERCENT

%token COMPONENT SITE PAGE STORE

%token <string> DECORATOR SYMBOL
%token <string> ID_LOWER
%token <string> ID_UPPER

%token EOF

%start <Ast.t option> program

%%

program:
  | d = declarations EOF {
      Some(d)
    }
  | EOF {
      None
    }

declarations:
  | d = declaration+;
    { { loc = $startpos; declarations = d } }

declaration:
  | decorators = decorator*; SITE; ident = ID_UPPER; LEFT_BRACE; RIGHT_BRACE; {
      Ast_Declaration.make_site decorators ident [] ""
    }
  | decorators = decorator*; COMPONENT; ident = ID_UPPER; LEFT_BRACE; RIGHT_BRACE; {
      Ast_Declaration.make_component decorators ident [] ""
    }
  | decorators = decorator*; PAGE; ident = ID_UPPER; LEFT_BRACE; RIGHT_BRACE; {
      Ast_Declaration.make_page decorators ident [] ""
    }
  | decorators = decorator*; STORE; ident = ID_UPPER; LEFT_BRACE; RIGHT_BRACE; {
      Ast_Declaration.make_store decorators ident []
    }

decorator:
  | ident = DECORATOR; LEFT_PAREN; attrs = separated_list(COMMA, attr); RIGHT_PAREN; {
      Ast_Decorator.make ident attrs
    }

attr:
  | name = ID_LOWER; COLON; value = expr; {
    Ast_Attr.make name value
  }

expr:
  | s = STRING                                                { Ast_Value.String s   }
  | i = INT                                                   { Ast_Value.Int i      }
  | TRUE                                                      { Ast_Value.Bool true  }
  | FALSE                                                     { Ast_Value.Bool false }
  | LEFT_BRACK; v = separated_list(COMMA, expr); RIGHT_BRACK  { Ast_Value.Array v    }
