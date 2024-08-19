module PincArray = struct
  let length ~arguments state =
    let array =
      arguments |> Helpers.Expect.(required (attribute "array" (array any_value)))
    in

    let result = Array.length array in

    let output = Helpers.Value.int result in
    State.add_output state ~output
  ;;
end

module PincString = struct
  let length ~arguments state =
    let str = arguments |> Helpers.Expect.(required (attribute "string" string)) in

    let result =
      str |> Containers.Utf8_string.of_string_exn |> Containers.Utf8_string.n_chars
    in

    let output = Helpers.Value.int result in
    State.add_output state ~output
  ;;

  let sub ~arguments state =
    let str = arguments |> Helpers.Expect.(required (attribute "string" string)) in
    let offset = arguments |> Helpers.Expect.(required (attribute "ofs" int)) in
    let length = arguments |> Helpers.Expect.(required (attribute "len" int)) in

    let result =
      str
      |> Containers.Utf8_string.of_string_exn
      |> Containers.Utf8_string.to_list
      |> Containers.List.drop offset
      |> Containers.List.take length
      |> Containers.Utf8_string.of_list
      |> Containers.Utf8_string.to_string
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
