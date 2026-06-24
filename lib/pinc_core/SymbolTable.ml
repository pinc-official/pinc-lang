type t = {
  store : symbol StringMap.t;
  length : Int32.t;
}

and symbol = {
  name : string;
  scope : scope;
  address : Int32.t;
}

and scope = Global

let make () = { store = StringMap.empty; length = Int32.zero }

let define_symbol t ~name =
  let address = Int32.succ t.length in
  let symbol = { name; scope = Global; address } in
  { store = StringMap.add name symbol t.store; length = address }
;;

let resolve_symbol t ~name = StringMap.find_opt name t.store
let length t = t.length
