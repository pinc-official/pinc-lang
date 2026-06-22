type t = {
  store : symbol StringMap.t;
  length : UInt16.t;
}

and symbol = {
  name : string;
  scope : scope;
  address : UInt16.t;
}

and scope = Global

let make () = { store = StringMap.empty; length = UInt16.make 0 }

let define_symbol t ~name =
  let symbol = { name; scope = Global; address = t.length } in
  { store = StringMap.add name symbol t.store; length = UInt16.succ t.length }
;;

let resolve_symbol t ~name = StringMap.find_opt name t.store
let length t = t.length
