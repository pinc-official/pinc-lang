include Map.Make (String)

let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    t
    |> map @@ fun x ->
       let new_acc, y = f !acc x in
       acc := new_acc;
       y
  in
  (!acc, result)
;;

let fold_mapi t ~init ~f =
  let acc = ref init in
  let result =
    t
    |> mapi @@ fun i x ->
       let new_acc, y = f i !acc x in
       acc := new_acc;
       y
  in
  (!acc, result)
;;
