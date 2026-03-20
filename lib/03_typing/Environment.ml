type t = Types.Scheme.t StringMap.t

let empty = StringMap.empty
let remove t var = StringMap.remove var t

let free_type_variables (t : t) =
  StringMap.fold
    (fun _key scheme acc -> StringSet.union (Types.Scheme.free_type_variables scheme) acc)
    t
    StringSet.empty
;;

let apply substitution t = StringMap.map (Types.Scheme.apply substitution) t
