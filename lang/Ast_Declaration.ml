type site = {
  decorators : Ast_Decorator.t list;
  ident : string;
  properties : string list;
  template : string;
}

type page = {
  decorators : Ast_Decorator.t list;
  ident : string;
  properties : string list;
  template : string;
}

type component = {
  decorators : Ast_Decorator.t list;
  ident : string;
  properties : string list;
  template : string;
}

type store = {
  decorators : Ast_Decorator.t list;
  ident : string;
  properties : string list;
}

type t = Site of site | Page of page | Component of component | Store of store

let make_store decorators ident properties =
  Store { decorators; ident; properties }

let make_component decorators ident properties template =
  Component { decorators; ident; properties; template }

let make_page decorators ident properties template =
  Page { decorators; ident; properties; template }

let make_site decorators ident properties template =
  Site { decorators; ident; properties; template }
