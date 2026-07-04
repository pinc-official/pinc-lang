module PincArray = struct
  let length ~arguments =
    let array =
      match arguments with
      | [ Value.Array a ] -> a
      | [ _ ] ->
          raise_notrace
            (Invalid_argument
               "The argument given to the Array.length function is not of type array")
      | _ ->
          raise_notrace
            (Invalid_argument
               "The Array.length function expects one argument (the array to compute the \
                length of).")
    in

    Value.Int (Array.length array)
  ;;
end

module PincString = struct
  let length ~arguments =
    let string =
      match arguments with
      | [ Value.String a ] -> a
      | [ _ ] ->
          raise_notrace
            (Invalid_argument
               "The argument given to the String.length function is not of type string")
      | _ ->
          raise_notrace
            (Invalid_argument
               "The String.length function expects one argument (the string to compute \
                the length of).")
    in

    Value.Int (String.length string)
  ;;

  let sub ~arguments =
    let string, offset, length =
      match arguments with
      | [ Value.String string; Value.Int offset; Value.Int length ] ->
          (string, offset, length)
      | [ _; Value.Int _; Value.Int _ ] ->
          raise_notrace
            (Invalid_argument
               "The first argument given to `String.sub` is not of type string")
      | [ Value.String _; _; Value.Int _ ] ->
          raise_notrace
            (Invalid_argument
               "The second argument (offset) given to `String.sub` is not of type int")
      | [ Value.String _; Value.Int _; _ ] ->
          raise_notrace
            (Invalid_argument
               "The third argument (length) given to `String.sub` is not of type int")
      | _ ->
          raise_notrace
            (Invalid_argument
               "The String.sub function must recieve three arguments (string, offset, \
                length).")
    in

    let result =
      string
      |> Pinc_Core.Utf8String.of_string_exn
      |> Pinc_Core.Utf8String.to_list
      |> List.drop offset
      |> List.take length
      |> Pinc_Core.Utf8String.of_list
      |> Pinc_Core.Utf8String.to_string
    in

    Value.String result
  ;;
end

let all =
  [|
    ("pinc_array_length", (1, PincArray.length));
    ("pinc_string_length", (1, PincString.length));
    ("pinc_string_sub", (3, PincString.sub));
  |]
;;

let find_index name = Array.find_index (fun (name', _) -> String.equal name name') all
let all = Array.map snd all
let expected_parameters index = fst @@ all.(index)
let get_function index = snd @@ all.(index)
