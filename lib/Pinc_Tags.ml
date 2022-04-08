open Pinc_Interpreter_Generic.Tag

let make ~eval ~validate ~transform = { validate; transform; eval }

module Default = struct
  let transform info value =
    let _, _, _, transformer = info in
    value |> transformer
  ;;

  let string =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Int _ -> Result.error "tried to assign integer value to a string tag."
      | `Float _ -> Result.error "tried to assign float value to a string tag."
      | `Bool _ -> Result.error "tried to assign boolean value to a string tag."
      | `Array _ -> Result.error "tried to assign array value to a string tag."
      | `Record _ -> Result.error "tried to assign record value to a string tag."
      | `TemplateNode _ -> Result.error "tried to assign template node to a string tag."
      | `DefinitionInfo _ ->
        Result.error "tried to assign definition info to a string tag."
      | `Function _ -> Result.error "tried to assign function to a string tag."
      | (`Null | `String _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #String"
        | Some (`String s) -> s
        | Some _ -> failwith "Expected attribute `key` #String to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      match state.Pinc_Interpreter_Generic.State.parent_component with
      | None -> failwith "Not Implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes
        |> StringMap.find_opt key
        |> (function
             | None -> default
             | Some v -> Some v)
        |> Option.value ~default:`Null
        |> validate info
        |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let int =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Float _ -> Result.error "tried to assign float value to a int tag."
      | `Bool _ -> Result.error "tried to assign boolean value to a int tag."
      | `Array _ -> Result.error "tried to assign array value to a int tag."
      | `String _ -> Result.error "tried to assign string value to a int tag."
      | `Record _ -> Result.error "tried to assign record value to a int tag."
      | `TemplateNode _ -> Result.error "tried to assign template node to a int tag."
      | `DefinitionInfo _ -> Result.error "tried to assign definition info to a int tag."
      | `Function _ -> Result.error "tried to assign function to a int tag."
      | (`Null | `Int _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Int"
        | Some (`String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Int to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      match state.Pinc_Interpreter_Generic.State.parent_component with
      | None -> failwith "Not Implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes
        |> StringMap.find_opt key
        |> (function
             | None -> default
             | Some v -> Some v)
        |> Option.value ~default:`Null
        |> validate info
        |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let float =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Bool _ -> Result.error "tried to assign boolean value to a float tag."
      | `Array _ -> Result.error "tried to assign array value to a float tag."
      | `String _ -> Result.error "tried to assign string value to a float tag."
      | `Int _ -> Result.error "tried to assign int value to a float tag."
      | `Record _ -> Result.error "tried to assign record value to a float tag."
      | `TemplateNode _ -> Result.error "tried to assign template node to a float tag."
      | `DefinitionInfo _ ->
        Result.error "tried to assign definition info to a float tag."
      | `Function _ -> Result.error "tried to assign function to a float tag."
      | (`Null | `Float _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Float"
        | Some (`String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Float to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      match state.Pinc_Interpreter_Generic.State.parent_component with
      | None -> failwith "Not Implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes
        |> StringMap.find_opt key
        |> (function
             | None -> default
             | Some v -> Some v)
        |> Option.value ~default:`Null
        |> validate info
        |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let boolean =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Array _ -> Result.error "tried to assign array value to a boolean tag."
      | `String _ -> Result.error "tried to assign string value to a boolean tag."
      | `Int _ -> Result.error "tried to assign int value to a boolean tag."
      | `Float _ -> Result.error "tried to assign float value to a boolean tag."
      | `Record _ -> Result.error "tried to assign record value to a boolean tag."
      | `TemplateNode _ -> Result.error "tried to assign template node to a boolean tag."
      | `DefinitionInfo _ ->
        Result.error "tried to assign definition info to a boolean tag."
      | `Function _ -> Result.error "tried to assign function to a boolean tag."
      | (`Null | `Bool _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Boolean"
        | Some (`String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Boolean to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      match state.Pinc_Interpreter_Generic.State.parent_component with
      | None -> failwith "Not Implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes
        |> StringMap.find_opt key
        |> (function
             | None -> default
             | Some v -> Some v)
        |> Option.value ~default:`Null
        |> validate info
        |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let array =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Bool _ -> Result.error "tried to assign boolean value to a array tag."
      | `String _ -> Result.error "tried to assign string value to a array tag."
      | `Int _ -> Result.error "tried to assign int value to a array tag."
      | `Float _ -> Result.error "tried to assign float value to a array tag."
      | `Record _ -> Result.error "tried to assign record value to a array tag."
      | `TemplateNode _ -> Result.error "tried to assign template node to a array tag."
      | `DefinitionInfo _ ->
        Result.error "tried to assign definition info to a array tag."
      | `Function _ -> Result.error "tried to assign function to a array tag."
      | (`Null | `Array _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Array"
        | Some (`String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Array to be of type string"
      in
      let of_info =
        StringMap.find_opt "of" attributes
        |> function
        | None -> failwith "Expected attribute `of` to exist on #Array"
        | Some (`TagInfo i) -> i
        | Some _ ->
          failwith
            "Expected attribute `of` #Array to be a tag describing the type of the items \
             inside."
      in
      let of_name, _, _, _ = of_info in
      let of_name = "#" ^ of_name in
      let of_handler =
        state.Pinc_Interpreter_Generic.State.tag_listeners
        |> StringMap.find_opt of_name
        |> function
        | None -> failwith ("No tag handler for `" ^ of_name ^ "` was provided.")
        | Some handler -> handler
      in
      let default = StringMap.find_opt "default" attributes in
      let validated_data =
        match state.Pinc_Interpreter_Generic.State.parent_component with
        | None -> failwith "Not Implemented!"
        | Some (_tag, tag_attributes, _tag_children) ->
          tag_attributes
          |> StringMap.find_opt key
          |> (function
               | None -> default
               | Some v -> Some v)
          |> Option.value ~default:`Null
          |> validate info
      in
      Result.bind validated_data (function
          | `Array a ->
            let rec loop acc list =
              match acc, list with
              | Error e, _ -> Result.error e
              | acc, [] -> acc
              | Ok acc, hd :: tl ->
                (match
                   hd
                   |> of_handler.validate of_info
                   |> Result.map (of_handler.transform of_info)
                 with
                | Ok v ->
                  let acc = Result.ok (v :: acc) in
                  loop acc tl
                | Error e -> Result.error e)
            in
            a
            |> Array.to_list
            |> loop (Result.ok [])
            |> Result.map List.rev
            |> Result.map Array.of_list
            |> Result.map (fun a -> `Array a)
          | `Null -> Result.ok `Null)
      |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let record =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Bool _ -> Result.error "tried to assign boolean value to a record tag."
      | `String _ -> Result.error "tried to assign string value to a record tag."
      | `Int _ -> Result.error "tried to assign int value to a record tag."
      | `Float _ -> Result.error "tried to assign float value to a record tag."
      | `Array _ -> Result.error "tried to assign array value to a record tag."
      | `TemplateNode _ -> Result.error "tried to assign template node to a record tag."
      | `DefinitionInfo _ ->
        Result.error "tried to assign definition info to a record tag."
      | `Function _ -> Result.error "tried to assign function to a record tag."
      | (`Null | `Record _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
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
      match state.Pinc_Interpreter_Generic.State.parent_component with
      | None -> failwith "Not Implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes
        |> StringMap.find_opt key
        |> (function
             | None -> default
             | Some v -> Some v)
        |> Option.value ~default:`Null
        |> validate info
        |> Result.map (function
               | `Record r ->
                 `Record
                   (r
                   |> StringMap.mapi (fun key value ->
                          of'
                          |> StringMap.find_opt key
                          |> function
                          | Some of_info ->
                            let of_name, _, _, _ = of_info in
                            let of_name = "#" ^ of_name in
                            let of_handler =
                              state.Pinc_Interpreter_Generic.State.tag_listeners
                              |> StringMap.find_opt of_name
                              |> function
                              | None ->
                                failwith
                                  ("No tag handler for `" ^ of_name ^ "` was provided.")
                              | Some handler -> handler
                            in
                            value
                            |> of_handler.validate of_info
                            |> (function
                            | Error e -> failwith e
                            | Ok value -> value |> of_handler.transform of_info)
                          | None -> value))
               | `Null -> `Null)
        |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let slot =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Bool _ -> Result.error "tried to assign boolean value to slot."
      | `String _ -> Result.error "tried to assign string value to slot."
      | `Int _ -> Result.error "tried to assign int value to slot."
      | `Float _ -> Result.error "tried to assign float value to slot."
      | `Record _ -> Result.error "tried to assign record value to slot."
      | `TemplateNode _ -> Result.error "tried to assign template node to slot."
      | `DefinitionInfo _ -> Result.error "tried to assign definition info to slot."
      | `Function _ -> Result.error "tried to assign function to slot."
      | (`Null | `Array _) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
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
      let slotted_children =
        match state.Pinc_Interpreter_Generic.State.parent_component with
        | None -> failwith "Not Implemented!"
        | Some (_tag, _tag_attributes, tag_children) ->
          tag_children |> Array.of_list |> Array.fold_left keep_slotted [||]
      in
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
      | _ -> `Array slotted_children |> validate info |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let set_context =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Bool _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `String _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Int _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Float _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Array _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `TemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `DefinitionInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Function _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Record _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | `Null -> Result.ok `Null
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
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
      Hashtbl.add state.Pinc_Interpreter_Generic.State.context name value;
      `Null |> validate info |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;

  let get_context =
    let validate _info value =
      match value with
      | `TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | ( `Bool _
        | `String _
        | `Int _
        | `Float _
        | `Array _
        | `TemplateNode _
        | `DefinitionInfo _
        | `Function _
        | `Record _
        | `Null ) as v -> Result.ok v
    in
    let eval state info =
      let _name, _optional, attributes, _transformer = info in
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
      Hashtbl.find_opt state.Pinc_Interpreter_Generic.State.context name
      |> (function
           | None -> default
           | Some v -> Some v)
      |> Option.value ~default:`Null
      |> validate info
      |> Result.map (transform info)
    in
    { validate; transform; eval }
  ;;
end