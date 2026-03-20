include Set.Make (String)

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
