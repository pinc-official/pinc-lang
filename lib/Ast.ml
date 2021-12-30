module rec Symbol: sig
  type ident = string
  type t = {
    ident: ident;
    attrs: Attr.t list;
  }
end = Symbol

and Value: sig
  type t = 
    | String of string 
    | Int of int 
    | Float of float 
    | Bool of bool 
    | Array of t list
    | Symbol of Symbol.t
end = Value

and Attr: sig
  type t = {
    name: string;
    value: Value.t
  }
end = Attr

and Declaration: sig
  type site = {
    symbols: Symbol.t list;
    ident: string;
    properties: string list;
    template: string;
  }

  type page = {
    symbols: Symbol.t list;
    ident: string;
    properties: string list;
    template: string;
  }

  type component = {
    symbols: Symbol.t list;
    ident: string;
    properties: string list;
    template: string;
  }

  type store = {
    symbols: Symbol.t list;
    ident: string;
    properties: string list;
  }

  type t = Site of site | Page of page | Component of component | Store of store
end = Declaration


type t = {
  loc: Position.t;
  declarations: Declaration.t list
}
