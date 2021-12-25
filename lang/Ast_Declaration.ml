type site = {
  symbols : Ast_Decorator.t list;
  ident : string;
  properties : string list;
  template : string;
}

type page = {
  symbols : Ast_Decorator.t list;
  ident : string;
  properties : string list;
  template : string;
}

type component = {
  symbols : Ast_Decorator.t list;
  ident : string;
  properties : string list;
  template : string;
}

type store = {
  symbols : Ast_Decorator.t list;
  ident : string;
  properties : string list;
}

type t = Site of site | Page of page | Component of component | Store of store

let make_store symbols ident properties =
  Store { symbols; ident; properties }

let make_component symbols ident properties template =
  Component { symbols; ident; properties; template }

let make_page symbols ident properties template =
  Page { symbols; ident; properties; template }

let make_site symbols ident properties template =
  Site { symbols; ident; properties; template }