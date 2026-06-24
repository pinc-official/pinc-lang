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
  let symbol = { name; scope = Global; address = t.length } in
  { store = StringMap.add name symbol t.store; length = Int32.succ t.length }
;;

let resolve_symbol t ~name = StringMap.find_opt name t.store
let length t = t.length
