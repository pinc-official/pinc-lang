module Scope = struct
  type t =
    | Global
    | Local
end

module Symbol = struct
  type t = {
    name : string;
    scope : Scope.t;
    address : Int32.t;
  }

  let make ~name ~scope ~address = { name; scope; address }
  let name t = t.name
  let scope t = t.scope
  let address t = t.address
end

type t = {
  store : Symbol.t StringMap.t;
  num_bindings : Int32.t;
  outer : t option;
}

let make () = { store = StringMap.empty; num_bindings = Int32.zero; outer = None }

let add_scope t =
  let t' = make () in
  { t' with outer = Some t }
;;

let pop_scope t =
  match t.outer with
  | None -> assert false
  | Some t -> t
;;

let define_symbol t ~name =
  let scope =
    match t.outer with
    | None -> Scope.Global
    | Some _ -> Scope.Local
  in
  let symbol = Symbol.make ~name ~scope ~address:t.num_bindings in
  let t' =
    {
      t with
      store = StringMap.add name symbol t.store;
      num_bindings = Int32.succ t.num_bindings;
    }
  in
  (t', symbol)
;;

let rec resolve_symbol t ~name =
  let symbol = StringMap.find_opt name t.store in
  match (symbol, t.outer) with
  | None, Some outer -> resolve_symbol outer ~name
  | _ -> symbol
;;

let length t = Int32.to_int t.num_bindings
