  $ esy x print_tokens ./data.fe
  Token.KEYWORD_SITE
  Token.IDENT_UPPER: ..... Docs
  Token.LEFT_PAREN
  Token.IDENT_LOWER: ..... label
  Token.COLON
  Token.STRING: .......... Docs Site
  Token.COMMA
  Token.IDENT_LOWER: ..... icon
  Token.COLON
  Token.STRING: .......... /images/icons/site-docs.svg
  Token.COMMA
  Token.IDENT_LOWER: ..... domain
  Token.COLON
  Token.STRING: .......... docs.fennek-cms.com
  Token.COMMA
  Token.IDENT_LOWER: ..... additional_domains
  Token.COLON
  Token.LEFT_BRACK
  Token.STRING: .......... docs.fennek-cms.de
  Token.COMMA
  Token.STRING: .......... docs.fennek-cms.fr
  Token.COMMA
  Token.RIGHT_BRACK
  Token.RIGHT_PAREN
  Token.LEFT_BRACE
  Token.IDENT_LOWER: ..... floats
  Token.EQUAL
  Token.LEFT_BRACK
  Token.FLOAT: ........... 1000.462000
  Token.COMMA
  Token.FLOAT: ........... 1.000000
  Token.COMMA
  Token.FLOAT: ........... 1.433200
  Token.COMMA
  Token.FLOAT: ........... 0.500000
  Token.COMMA
  Token.FLOAT: ........... 10000000000.000000
  Token.COMMA
  Token.FLOAT: ........... 100.000000
  Token.COMMA
  Token.FLOAT: ........... 0.000010
  Token.COMMA
  Token.FLOAT: ........... 0.010000
  Token.COMMA
  Token.FLOAT: ........... 0.010000
  Token.RIGHT_BRACK
  Token.SEMICOLON
  Token.RIGHT_BRACE

  $ esy x print_ast ./data.fe
  ./data.fe:1:0
  site Docs
    attr label
      string Docs Site
    attr icon
      string /images/icons/site-docs.svg
    attr domain
      string docs.fennek-cms.com
    attr additional_domains
      array [
        string docs.fennek-cms.de
        string docs.fennek-cms.fr
      ]
