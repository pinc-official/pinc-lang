module PincArray = struct
  let length ~loc ~arguments state =
    let array =
      let open Helpers.Expect in
      arguments |> required (attribute "array" (array any_value))
    in

    let result =
      match array with
      | Error `Required ->
          Pinc_Diagnostics.raise_error
            loc
            (Printf.sprintf
               "The Array.length function expects one argument (the array to compute the \
                length of).")
      | Error (`UnexpectedType value_loc) ->
          Pinc_Diagnostics.raise_error
            value_loc
            (Printf.sprintf
               "The argument given to the Array.length function is not of type array")
      | Ok v -> Array.length v
    in

    let output = Helpers.Value.int result in
    State.add_output state ~output
  ;;
end

module PincString = struct
  let length ~loc ~arguments state =
    let str = arguments |> Helpers.Expect.(required (attribute "string" string)) in

    let result =
      match str with
      | Error `Required ->
          Pinc_Diagnostics.raise_error
            loc
            (Printf.sprintf
               "The String.length function expects one argument (the string to compute \
                the length of).")
      | Error (`UnexpectedType value_loc) ->
          Pinc_Diagnostics.raise_error
            value_loc
            (Printf.sprintf
               "The argument given to the String.length function is not of type string")
      | Ok v -> v |> Pinc_Core.Utf8String.of_string_exn |> Pinc_Core.Utf8String.n_chars
    in

    let output = Helpers.Value.int result in
    State.add_output state ~output
  ;;

  let sub ~loc ~arguments state =
    let str =
      (arguments |> Helpers.Expect.(required (attribute "string" string))) |> function
      | Error `Required ->
          Pinc_Diagnostics.raise_error
            loc
            (Printf.sprintf
               "The String.sub function must recieve three arguments (string, offset, \
                length).")
      | Error (`UnexpectedType value_loc) ->
          Pinc_Diagnostics.raise_error
            value_loc
            (Printf.sprintf
               "The first argument given to `String.sub` is not of type string")
      | Ok v -> v
    in
    let offset =
      (arguments |> Helpers.Expect.(required (attribute "ofs" int))) |> function
      | Error `Required ->
          Pinc_Diagnostics.raise_error
            loc
            (Printf.sprintf
               "The String.sub function must recieve three arguments (string, offset, \
                length).")
      | Error (`UnexpectedType value_loc) ->
          Pinc_Diagnostics.raise_error
            value_loc
            (Printf.sprintf
               "The second argument (offset) given to `String.sub` is not of type int")
      | Ok v -> v
    in
    let length =
      (arguments |> Helpers.Expect.(required (attribute "len" int))) |> function
      | Error `Required ->
          Pinc_Diagnostics.raise_error
            loc
            (Printf.sprintf
               "The String.sub function must recieve three arguments (string, offset, \
                length).")
      | Error (`UnexpectedType value_loc) ->
          Pinc_Diagnostics.raise_error
            value_loc
            (Printf.sprintf
               "The third argument (length) given to `String.sub` is not of type int")
      | Ok v -> v
    in

    let result =
      str
      |> Pinc_Core.Utf8String.of_string_exn
      |> Pinc_Core.Utf8String.to_list
      |> List.drop offset
      |> List.take length
      |> Pinc_Core.Utf8String.of_list
      |> Pinc_Core.Utf8String.to_string
    in

    let output = Helpers.Value.string result in
    State.add_output state ~output
  ;;
end

let all =
  StringMap.of_list
    [
      ("pinc_array_length", PincArray.length);
      ("pinc_string_length", PincString.length);
      ("pinc_string_sub", PincString.sub);
    ]
;;
