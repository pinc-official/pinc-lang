  $ esy x print_tokens ./data.fe
  Symbol: Ui
  (
  Lower Ident: label
  :
  String: "Docs Site"
  ,
  Lower Ident: icon
  :
  String: "/images/icons/site-docs.svg"
  )
  Symbol: Domain
  (
  Lower Ident: main
  :
  String: "docs.fennek-cms.com"
  ,
  Lower Ident: additional
  :
  [
  String: "docs.fennek-cms.de"
  ,
  String: "docs.fennek-cms.fr"
  ]
  )
  site
  Upper Ident: Docs
  {
  }

  $ esy x print ./data.fe
  ./data.fe:1:1
  site Docs
  symbol Ui
  attr label
  string Docs Site
  attr icon
  string /images/icons/site-docs.svg
  symbol Domain
  attr main
  string docs.fennek-cms.com
  attr additional
  string docs.fennek-cms.de
  string docs.fennek-cms.fr
