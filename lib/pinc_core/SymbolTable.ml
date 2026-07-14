module Kind = struct
  type t =
    | Global
    | Local
    | Free
    | Function
end

module Symbol = struct
  type t = {
    name : string;
    kind : Kind.t;
    address : int;
    is_mutable : bool;
    level : int;
  }

  let make ~name ~kind ~address ~level ~is_mutable =
    { name; kind; address; level; is_mutable }
  ;;

  let name t = t.name
  let kind t = t.kind
  let address t = t.address
  let level t = t.level
  let is_mutable t = t.is_mutable
end

type t = {
  store : Symbol.t StringMap.t;
  free_variables : Symbol.t list;
  num_bindings : int;
  outer : t option;
  mutable scope : int;
}

let make () =
  {
    store = StringMap.empty;
    free_variables = [];
    num_bindings = 0;
    outer = None;
    scope = 0;
  }
;;

let add_scope t =
  t.scope <- succ t.scope;
  t
;;

let pop_scope t =
  t.scope <- pred t.scope;
  t
;;

let add_frame t =
  let t' = make () in
  { t' with outer = Some t }
;;

let pop_frame t =
  match t.outer with
  | None -> assert false
  | Some t -> t
;;

let define_symbol t ~name ~is_mutable =
  let kind =
    match t.outer with
    | None -> Kind.Global
    | Some _ -> Kind.Local
  in
  let symbol =
    Symbol.make ~name ~kind ~address:t.num_bindings ~level:t.scope ~is_mutable
  in
  let t' =
    {
      t with
      store = StringMap.add name symbol t.store;
      num_bindings = Int.succ t.num_bindings;
    }
  in
  (t', symbol)
;;

let define_free_symbol t symbol =
  let name = Symbol.name symbol in
  let is_mutable = Symbol.is_mutable symbol in
  let level = Symbol.level symbol in
  let free_symbol =
    Symbol.make
      ~name
      ~kind:Free
      ~address:(List.length t.free_variables)
      ~level
      ~is_mutable
  in
  let t' =
    {
      t with
      store = StringMap.add name free_symbol t.store;
      free_variables = t.free_variables @ [ symbol ];
    }
  in
  (t', free_symbol)
;;

let define_function_symbol t ~name =
  let symbol =
    Symbol.make ~name ~kind:Function ~address:0 ~level:t.scope ~is_mutable:false
  in
  let t' = { t with store = StringMap.add name symbol t.store } in
  (t', symbol)
;;

let rec resolve_symbol t ~name =
  let symbol = StringMap.find_opt name t.store in
  match (symbol, t.outer) with
  | Some symbol, _ ->
      if symbol.level <= t.scope then
        (t, Some symbol)
      else
        (t, None)
  | None, None -> (t, None)
  | None, Some outer ->
      begin match resolve_symbol outer ~name with
      | outer, None -> ({ t with outer = Some outer }, None)
      | outer, Some outer_symbol ->
          begin match Symbol.kind outer_symbol with
          | Global -> ({ t with outer = Some outer }, Some outer_symbol)
          | Local | Free | Function ->
              let t', free_symbol = define_free_symbol t outer_symbol in
              ({ t' with outer = Some outer }, Some free_symbol)
          end
      end
;;

let free_variables t = t.free_variables
let length t = t.num_bindings
