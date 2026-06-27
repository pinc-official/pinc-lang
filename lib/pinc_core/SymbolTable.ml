type t = {
  store : symbol StringMap.t;
  last_used_address : Int32.t;
}

and symbol = {
  name : string;
  scope : scope;
  address : Int32.t;
}

and scope = Global

let make () = { store = StringMap.empty; last_used_address = Int32.zero }

let define_symbol t ~name =
  let address = Int32.succ t.last_used_address in
  let symbol = { name; scope = Global; address } in
  ({ store = StringMap.add name symbol t.store; last_used_address = address }, address)
;;

let resolve_symbol t ~name = StringMap.find_opt name t.store
