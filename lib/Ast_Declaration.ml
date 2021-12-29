type site = {
  symbols : Ast_Symbol.t list;
  ident : string;
  properties : string list;
  template : string;
}

type page = {
  symbols : Ast_Symbol.t list;
  ident : string;
  properties : string list;
  template : string;
}

type component = {
  symbols : Ast_Symbol.t list;
  ident : string;
  properties : string list;
  template : string;
}

type store = {
  symbols : Ast_Symbol.t list;
  ident : string;
  properties : string list;
}

type t = Site of site | Page of page | Component of component | Store of store

let make_store ~symbols ~properties ident =
  Store { symbols; ident; properties }

let make_component ~symbols ~properties ~template ident =
  Component { symbols; ident; properties; template }

let make_page ~symbols ~properties ~template ident =
  Page { symbols; ident; properties; template }

let make_site ~symbols ~properties ~template ident =
  Site { symbols; ident; properties; template }
