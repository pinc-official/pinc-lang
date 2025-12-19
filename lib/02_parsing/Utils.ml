module Option = struct
  include Option

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
end

module List = struct
  include List

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
end

module Array = struct
  include Array

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
end
