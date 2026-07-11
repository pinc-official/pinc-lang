module Scope = struct
  type t =
    | Global
    | Local
    | Free
    | Function
end

module Symbol = struct
  type t = {
    name : string;
    scope : Scope.t;
    address : int;
    is_mutable : bool;
  }

  let make ~name ~scope ~address ~is_mutable = { name; scope; address; is_mutable }
  let name t = t.name
  let scope t = t.scope
  let address t = t.address
  let is_mutable t = t.is_mutable
end

type t = {
  store : Symbol.t StringMap.t;
  free_variables : Symbol.t list;
  num_bindings : int;
  outer : t option;
}

let make () =
  { store = StringMap.empty; free_variables = []; num_bindings = 0; outer = None }
;;

let add_scope t =
  let t' = make () in
  { t' with outer = Some t }
;;

let pop_scope t =
  match t.outer with
  | None -> assert false
  | Some t -> t
;;

let define_symbol t ~name ~is_mutable =
  let scope =
    match t.outer with
    | None -> Scope.Global
    | Some _ -> Scope.Local
  in
  let symbol = Symbol.make ~name ~scope ~address:t.num_bindings ~is_mutable in
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
  let free_symbol =
    Symbol.make ~name ~scope:Free ~address:(List.length t.free_variables) ~is_mutable
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
  let symbol = Symbol.make ~name ~scope:Function ~address:0 ~is_mutable:false in
  let t' = { t with store = StringMap.add name symbol t.store } in
  (t', symbol)
;;

let rec resolve_symbol t ~name =
  let symbol = StringMap.find_opt name t.store in
  match (symbol, t.outer) with
  | Some symbol, _ -> (t, Some symbol)
  | None, None -> (t, None)
  | None, Some outer ->
      begin match resolve_symbol outer ~name with
      | outer, None -> ({ t with outer = Some outer }, None)
      | outer, Some outer_symbol ->
          begin match Symbol.scope outer_symbol with
          | Global -> ({ t with outer = Some outer }, Some outer_symbol)
          | Local | Free | Function ->
              let t', free_symbol = define_free_symbol t outer_symbol in
              ({ t' with outer = Some outer }, Some free_symbol)
          end
      end
;;

let free_variables t = t.free_variables
let length t = t.num_bindings
