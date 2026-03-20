type t = Types.Type.t StringMap.t

let empty = StringMap.empty

let compose s1 s2 =
  let s3 = StringMap.map (Types.Type.apply s1) s2 in
  StringMap.union (fun _ a _ -> Some a) s1 s3
;;
