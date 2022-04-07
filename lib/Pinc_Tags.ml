module StringMap = Pinc_Ast.StringMap

module StringTag = struct
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Int _ -> failwith "tried to assign integer value to a string tag."
    | `Float _ -> failwith "tried to assign float value to a string tag."
    | `Bool _ -> failwith "tried to assign boolean value to a string tag."
    | `Array _ -> failwith "tried to assign array value to a string tag."
    | `Record _ -> failwith "tried to assign record value to a string tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a string tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a string tag."
    | `Function _ -> failwith "tried to assign function to a string tag."
    | `Null -> `Null
    | `String s -> `String s
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let key =
      StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #String"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #String to be of type string"
    in
    let default = StringMap.find_opt "default" attributes in
    data
    |> StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> transformer
  ;;
end

module IntTag = struct
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Float _ -> failwith "tried to assign float value to a int tag."
    | `Bool _ -> failwith "tried to assign boolean value to a int tag."
    | `Array _ -> failwith "tried to assign array value to a int tag."
    | `String _ -> failwith "tried to assign string value to a int tag."
    | `Record _ -> failwith "tried to assign record value to a int tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a int tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a int tag."
    | `Function _ -> failwith "tried to assign function to a int tag."
    | `Null -> `Null
    | `Int i -> `Int i
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let key =
      StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Int"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Int to be of type string"
    in
    let default = StringMap.find_opt "default" attributes in
    data
    |> StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> transformer
  ;;
end

module FloatTag = struct
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Bool _ -> failwith "tried to assign boolean value to a float tag."
    | `Array _ -> failwith "tried to assign array value to a float tag."
    | `String _ -> failwith "tried to assign string value to a float tag."
    | `Int _ -> failwith "tried to assign int value to a float tag."
    | `Record _ -> failwith "tried to assign record value to a float tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a float tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a float tag."
    | `Function _ -> failwith "tried to assign function to a float tag."
    | `Null -> `Null
    | `Float f -> `Float f
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let key =
      StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Float"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Float to be of type string"
    in
    let default = StringMap.find_opt "default" attributes in
    data
    |> StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> transformer
  ;;
end

module BooleanTag = struct
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Array _ -> failwith "tried to assign array value to a boolean tag."
    | `String _ -> failwith "tried to assign string value to a boolean tag."
    | `Int _ -> failwith "tried to assign int value to a boolean tag."
    | `Float _ -> failwith "tried to assign float value to a boolean tag."
    | `Record _ -> failwith "tried to assign record value to a boolean tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a boolean tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a boolean tag."
    | `Function _ -> failwith "tried to assign function to a boolean tag."
    | `Null -> `Null
    | `Bool b -> `Bool b
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let key =
      StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Boolean"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Boolean to be of type string"
    in
    let default = StringMap.find_opt "default" attributes in
    data
    |> StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> transformer
  ;;
end

module ArrayTag = struct
  (* TODO: Validate the underlying values with their tags validator *)
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Bool _ -> failwith "tried to assign boolean value to a array tag."
    | `String _ -> failwith "tried to assign string value to a array tag."
    | `Int _ -> failwith "tried to assign int value to a array tag."
    | `Float _ -> failwith "tried to assign float value to a array tag."
    | `Record _ -> failwith "tried to assign record value to a array tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a array tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a array tag."
    | `Function _ -> failwith "tried to assign function to a array tag."
    | `Null -> `Null
    | `Array a -> `Array a
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let key =
      StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Array"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Array to be of type string"
    in
    let of' =
      StringMap.find_opt "of" attributes
      |> function
      | None -> failwith "Expected attribute `of` to exist on #Array"
      | Some (`TagInfo (_name, _optional, _attributes, transformer)) -> transformer
      | Some _ ->
        failwith
          "Expected attribute `of` #Array to be a tag describing the type of the items \
           inside."
    in
    let default = StringMap.find_opt "default" attributes in
    data
    |> StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> (function
         | `Array a -> `Array (a |> Array.map (fun value -> of' value))
         | `Null -> `Null)
    |> transformer
  ;;
end

module RecordTag = struct
  (* TODO: Validate the underlying values with their tags validator *)
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Bool _ -> failwith "tried to assign boolean value to a record tag."
    | `String _ -> failwith "tried to assign string value to a record tag."
    | `Int _ -> failwith "tried to assign int value to a record tag."
    | `Float _ -> failwith "tried to assign float value to a record tag."
    | `Array _ -> failwith "tried to assign array value to a record tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a record tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a record tag."
    | `Function _ -> failwith "tried to assign function to a record tag."
    | `Null -> `Null
    | `Record r -> `Record r
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let key =
      StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Record"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Record to be of type string"
    in
    let of' =
      StringMap.find_opt "of" attributes
      |> function
      | None -> failwith "Expected attribute `of` to exist on #Record"
      | Some (`Record r) ->
        r
        |> StringMap.filter_map (fun _key -> function
             | `TagInfo i -> Some i
             | _ -> None)
      | Some _ ->
        failwith
          "Expected attribute `of` #Array to be a tag describing the type of the items \
           inside."
    in
    let default = StringMap.find_opt "default" attributes in
    data
    |> StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> (function
         | `Record r ->
           `Record
             (r
             |> StringMap.mapi (fun key value ->
                    of'
                    |> StringMap.find_opt key
                    |> function
                    | Some (_name, _optional, _attributes, transformer) ->
                      transformer value
                    | None -> value))
         | `Null -> `Null)
    |> transformer
  ;;
end

module SlotTag = struct
  let get_value data (_name, _optional, attributes, transformer) =
    let slot_name =
      attributes
      |> StringMap.find_opt "name"
      |> Option.value ~default:(`String "")
      |> function
      | `String s -> s
      | _ -> failwith "Expected attribute `name` on #Slot to be of type string."
    in
    let min =
      attributes
      |> StringMap.find_opt "min"
      |> Option.value ~default:(`Int 0)
      |> function
      | `Int i -> i
      | _ -> failwith "Expected attribute `min` on #Slot to be of type int."
    in
    let max =
      attributes
      |> StringMap.find_opt "max"
      |> function
      | None -> None
      | Some (`Int i) -> Some i
      | _ -> failwith "Expected attribute `max` on #Slot to be of type int."
    in
    let instanceOf =
      attributes
      |> StringMap.find_opt "instanceOf"
      |> function
      | None -> None
      | Some (`Array l) ->
        Some
          (l
          |> Array.map (function
                 | `DefinitionInfo info -> info
                 | _ ->
                   failwith
                     "Expected attribute `instanceOf` on #Slot to be an array of \
                      uppercase identifiers."))
      | _ -> failwith "Expected attribute `instanceOf` on #Slot to be an array."
    in
    let find_slot_key attributes =
      attributes
      |> StringMap.find_opt "slot"
      |> Option.value ~default:(`String "")
      |> function
      | `String s -> s
      | _ -> failwith "Expected slot attribute to be of type string"
    in
    let check_instance_restriction tag f =
      match instanceOf with
      | None -> f
      | Some restrictions ->
        let is_in_list = ref false in
        let allowed, disallowed =
          restrictions
          |> Array.to_list
          |> List.partition_map (fun (name, _exists, negated) ->
                 if name = tag then is_in_list := true;
                 match negated with
                 | `Negated -> Either.right name
                 | `NotNegated -> Either.left name)
        in
        let is_in_list = !is_in_list in
        let is_allowed =
          match allowed, disallowed with
          | [], _disallowed -> not is_in_list
          | _allowed, [] -> is_in_list
          | allowed, _disallowed -> List.mem tag allowed
        in
        if not is_allowed
        then
          failwith
            ("Child with tag `"
            ^ tag
            ^ "` may not be used inside the "
            ^ (if slot_name = ""
              then "Default #Slot."
              else "#Slot with name `" ^ slot_name ^ "`")
            ^ ". The following restrictions are set: [ "
            ^ (instanceOf
              |> Option.value ~default:[||]
              |> Array.to_list
              |> List.map (fun (name, _exists, negated) ->
                     match negated with
                     | `Negated -> "!" ^ name
                     | `NotNegated -> name)
              |> String.concat ",")
            ^ " ]")
        else f
    in
    let rec keep_slotted acc = function
      | ( `TemplateNode (`Html, tag, attributes, _children, _self_closing)
        | `TemplateNode (`Component _, tag, attributes, _children, _self_closing) ) as
        value ->
        if find_slot_key attributes = slot_name
        then check_instance_restriction tag @@ Array.append acc [| value |]
        else acc
      | `Array l -> l |> Array.fold_left keep_slotted acc
      | `String s when String.trim s = "" -> acc
      | _ ->
        failwith
          "Only nodes may be placed into slots. If you want to put a plain text into a \
           slot, you have to wrap it in a <p></p> tag for example."
    in
    let slotted_children = data |> Array.of_list |> Array.fold_left keep_slotted [||] in
    let amount_of_children = Array.length slotted_children in
    match slot_name, min, amount_of_children, max with
    | "", min, len, _ when len < min ->
      failwith
        ("Default #Slot did not reach the minimum amount of nodes (specified as "
        ^ string_of_int min
        ^ ").")
    | slot_name, min, len, _ when len < min ->
      failwith
        ("#Slot with name `"
        ^ slot_name
        ^ "` did not reach the minimum amount of nodes (specified as "
        ^ string_of_int min
        ^ ").")
    | "", _, len, Some max when len > max ->
      failwith
        ("Default #Slot includes more than the maximum amount of nodes (specified as "
        ^ string_of_int max
        ^ ").")
    | slot_name, _, len, Some max when len > max ->
      failwith
        ("#Slot with name `"
        ^ slot_name
        ^ "` includes more than the maximum amount of nodes (specified as "
        ^ string_of_int max
        ^ ").")
    | _ -> `Array slotted_children |> transformer
  ;;
end

module SetContextTag = struct
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Bool _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `String _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Int _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Float _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Array _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `TemplateNode _ ->
      failwith "Something unexpected happened...this is my fault, not yours"
    | `DefinitionInfo _ ->
      failwith "Something unexpected happened...this is my fault, not yours"
    | `Function _ ->
      failwith "Something unexpected happened...this is my fault, not yours"
    | `Record _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | `Null -> `Null
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let name =
      attributes
      |> StringMap.find_opt "name"
      |> function
      | Some (`String s) -> s
      | None -> failwith "attribute name is required when setting a context."
      | Some _ ->
        failwith "Expected attribute `name` on #SetContext to be of type string."
    in
    let value =
      attributes
      |> StringMap.find_opt "value"
      |> function
      | Some value -> value
      | None -> failwith "attribute value is required when setting a context."
    in
    Hashtbl.add data name value;
    `Null |> validate |> transformer
  ;;
end

module GetContextTag = struct
  let validate = function
    | `TagInfo _ -> failwith "Something unexpected happened...this is my fault, not yours"
    | ( `Bool _
      | `String _
      | `Int _
      | `Float _
      | `Array _
      | `TemplateNode _
      | `DefinitionInfo _
      | `Function _
      | `Record _
      | `Null ) as v -> v
  ;;

  let get_value data (_name, _optional, attributes, transformer) =
    let name =
      attributes
      |> StringMap.find_opt "name"
      |> function
      | Some (`String s) -> s
      | None -> failwith "attribute name is required when getting a context."
      | Some _ ->
        failwith "Expected attribute `name` on #GetContext to be of type string."
    in
    let default = StringMap.find_opt "default" attributes in
    Hashtbl.find_opt data name
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:`Null
    |> validate
    |> transformer
  ;;
end
