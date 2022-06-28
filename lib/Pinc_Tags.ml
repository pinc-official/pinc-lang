open Pinc_Interpreter_Types

let make ~eval ~validate ~transform ~get_value = { validate; transform; eval; get_value }

module Default = struct
  let transform info value =
    let _, _, _, transformer = info in
    value |> transformer
  ;;

  let string =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Int _ -> Result.error "tried to assign integer value to a string tag."
      | Float _ -> Result.error "tried to assign float value to a string tag."
      | Bool _ -> Result.error "tried to assign boolean value to a string tag."
      | Array _ -> Result.error "tried to assign array value to a string tag."
      | Record _ -> Result.error "tried to assign record value to a string tag."
      | HtmlTemplateNode _ ->
        Result.error "tried to assign template node to a string tag."
      | ComponentTemplateNode _ ->
        Result.error "tried to assign template node to a string tag."
      | DefinitionInfo _ ->
        Result.error "tried to assign definition info to a string tag."
      | Function _ -> Result.error "tried to assign function to a string tag."
      | (Null | String _) as v -> Result.ok v
    in
    let get_value state key =
      match state.parent_component with
      | None -> failwith "#String is not implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes |> StringMap.find_opt key
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #String"
        | Some (String s) -> s
        | Some _ -> failwith "Expected attribute `key` #String to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      key
      |> self.get_value state
      |> (function
           | None | Some Null -> default
           | Some v -> Some v)
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let int =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Float _ -> Result.error "tried to assign float value to a int tag."
      | Bool _ -> Result.error "tried to assign boolean value to a int tag."
      | Array _ -> Result.error "tried to assign array value to a int tag."
      | String _ -> Result.error "tried to assign string value to a int tag."
      | Record _ -> Result.error "tried to assign record value to a int tag."
      | HtmlTemplateNode _ -> Result.error "tried to assign template node to a int tag."
      | ComponentTemplateNode _ ->
        Result.error "tried to assign template node to a int tag."
      | DefinitionInfo _ -> Result.error "tried to assign definition info to a int tag."
      | Function _ -> Result.error "tried to assign function to a int tag."
      | (Null | Int _) as v -> Result.ok v
    in
    let get_value state key =
      match state.parent_component with
      | None -> failwith "#Int is not implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes |> StringMap.find_opt key
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Int"
        | Some (String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Int to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      key
      |> self.get_value state
      |> (function
           | None | Some Null -> default
           | Some v -> Some v)
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let float =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ -> Result.error "tried to assign boolean value to a float tag."
      | Array _ -> Result.error "tried to assign array value to a float tag."
      | String _ -> Result.error "tried to assign string value to a float tag."
      | Int _ -> Result.error "tried to assign int value to a float tag."
      | Record _ -> Result.error "tried to assign record value to a float tag."
      | HtmlTemplateNode _ -> Result.error "tried to assign template node to a float tag."
      | ComponentTemplateNode _ ->
        Result.error "tried to assign template node to a float tag."
      | DefinitionInfo _ -> Result.error "tried to assign definition info to a float tag."
      | Function _ -> Result.error "tried to assign function to a float tag."
      | (Null | Float _) as v -> Result.ok v
    in
    let get_value state key =
      match state.parent_component with
      | None -> failwith "#Float is not implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes |> StringMap.find_opt key
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Float"
        | Some (String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Float to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      key
      |> self.get_value state
      |> (function
           | None | Some Null -> default
           | Some v -> Some v)
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let boolean =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Array _ -> Result.error "tried to assign array value to a boolean tag."
      | String _ -> Result.error "tried to assign string value to a boolean tag."
      | Int _ -> Result.error "tried to assign int value to a boolean tag."
      | Float _ -> Result.error "tried to assign float value to a boolean tag."
      | Record _ -> Result.error "tried to assign record value to a boolean tag."
      | HtmlTemplateNode _ ->
        Result.error "tried to assign template node to a boolean tag."
      | ComponentTemplateNode _ ->
        Result.error "tried to assign template node to a boolean tag."
      | DefinitionInfo _ ->
        Result.error "tried to assign definition info to a boolean tag."
      | Function _ -> Result.error "tried to assign function to a boolean tag."
      | (Null | Bool _) as v -> Result.ok v
    in
    let get_value state key =
      match state.parent_component with
      | None -> failwith "#Boolean is not implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes |> StringMap.find_opt key
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Boolean"
        | Some (String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Boolean to be of type string"
      in
      let default = StringMap.find_opt "default" attributes in
      key
      |> self.get_value state
      |> (function
           | None | Some Null -> default
           | Some v -> Some v)
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let array =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ -> Result.error "tried to assign boolean value to a array tag."
      | String _ -> Result.error "tried to assign string value to a array tag."
      | Int _ -> Result.error "tried to assign int value to a array tag."
      | Float _ -> Result.error "tried to assign float value to a array tag."
      | Record _ -> Result.error "tried to assign record value to a array tag."
      | HtmlTemplateNode _ -> Result.error "tried to assign template node to a array tag."
      | ComponentTemplateNode _ ->
        Result.error "tried to assign template node to a array tag."
      | DefinitionInfo _ -> Result.error "tried to assign definition info to a array tag."
      | Function _ -> Result.error "tried to assign function to a array tag."
      | (Null | Array _) as v -> Result.ok v
    in
    let get_value state key =
      match state.parent_component with
      | None -> failwith "#Array is not implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes |> StringMap.find_opt key
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Array"
        | Some (String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Array to be of type string"
      in
      let of_info =
        StringMap.find_opt "of" attributes
        |> function
        | None -> failwith "Expected attribute `of` to exist on #Array"
        | Some (TagInfo i) -> i
        | Some _ ->
          failwith
            "Expected attribute `of` #Array to be a tag describing the type of the items \
             inside."
      in
      let of_name, _, _, _ = of_info in
      let of_name = "#" ^ of_name in
      let of_handler =
        state.tag_listeners
        |> StringMap.find_opt of_name
        |> function
        | None -> failwith ("No tag handler for `" ^ of_name ^ "` was provided.")
        | Some handler -> handler
      in
      let default = StringMap.find_opt "default" attributes in
      let validated_data =
        key
        |> self.get_value state
        |> (function
             | None | Some Null -> default
             | Some v -> Some v)
        |> Option.value ~default:Null
        |> self.validate info
      in
      Result.bind validated_data (function
          | Array a ->
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
            |> Result.map (fun a -> Array a)
          | Null -> Result.ok Null
          | _ -> failwith "tried to assign non array value to array tag.")
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let record =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ -> Result.error "tried to assign boolean value to a record tag."
      | String _ -> Result.error "tried to assign string value to a record tag."
      | Int _ -> Result.error "tried to assign int value to a record tag."
      | Float _ -> Result.error "tried to assign float value to a record tag."
      | Array _ -> Result.error "tried to assign array value to a record tag."
      | HtmlTemplateNode _ ->
        Result.error "tried to assign template node to a record tag."
      | ComponentTemplateNode _ ->
        Result.error "tried to assign template node to a record tag."
      | DefinitionInfo _ ->
        Result.error "tried to assign definition info to a record tag."
      | Function _ -> Result.error "tried to assign function to a record tag."
      | (Null | Record _) as v -> Result.ok v
    in
    let get_value state key =
      match state.parent_component with
      | None -> failwith "#Record is not implemented!"
      | Some (_tag, tag_attributes, _tag_children) ->
        tag_attributes |> StringMap.find_opt key
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let key =
        StringMap.find_opt "key" attributes
        |> function
        | None -> failwith "Expected attribute `key` to exist on #Record"
        | Some (String s) -> s
        | Some _ -> failwith "Expected attribute `key` #Record to be of type string"
      in
      let of' =
        StringMap.find_opt "of" attributes
        |> function
        | None -> failwith "Expected attribute `of` to exist on #Record"
        | Some (Record r) ->
          r
          |> StringMap.filter_map (fun _key -> function
               | TagInfo i -> Some i
               | _ -> None)
        | Some _ ->
          failwith
            "Expected attribute `of` #Array to be a tag describing the type of the items \
             inside."
      in
      let default = StringMap.find_opt "default" attributes in
      key
      |> self.get_value state
      |> (function
           | None | Some Null -> default
           | Some v -> Some v)
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (function
             | Record r ->
               Record
                 (r
                 |> StringMap.mapi (fun key value ->
                        of'
                        |> StringMap.find_opt key
                        |> function
                        | Some of_info ->
                          let of_name, _, _, _ = of_info in
                          let of_name = "#" ^ of_name in
                          let of_handler =
                            state.tag_listeners
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
             | Null -> Null
             | _ -> failwith "Tried to assign non record value to a record tag.")
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let slot =
    let validate info value =
      let _name, _optional, attributes, _transformer = info in
      let min =
        attributes
        |> StringMap.find_opt "min"
        |> Option.value ~default:(Int 0)
        |> function
        | Int i -> i
        | _ -> failwith "Expected attribute `min` on #Slot to be of type int."
      in
      let max =
        attributes
        |> StringMap.find_opt "max"
        |> function
        | None -> Int.max_int
        | Some (Int i) -> i
        | _ -> failwith "Expected attribute `max` on #Slot to be of type int."
      in
      let constraints =
        attributes
        |> StringMap.find_opt "constraints"
        |> function
        | None -> None
        | Some (Array l) ->
          Some
            (l
            |> Array.map (function
                   | DefinitionInfo info -> info
                   | _ ->
                     failwith
                       "Expected attribute `constraints` on #Slot to be an array of \
                        uppercase identifiers."))
        | _ -> failwith "Expected attribute `constraints` on #Slot to be an array."
      in
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ -> Result.error "tried to assign boolean value to slot."
      | String _ -> Result.error "tried to assign string value to slot."
      | Int _ -> Result.error "tried to assign int value to slot."
      | Float _ -> Result.error "tried to assign float value to slot."
      | Record _ -> Result.error "tried to assign record value to slot."
      | HtmlTemplateNode _ -> Result.error "tried to assign template node to slot."
      | ComponentTemplateNode _ -> Result.error "tried to assign template node to slot."
      | DefinitionInfo _ -> Result.error "tried to assign definition info to slot."
      | Function _ -> Result.error "tried to assign function to slot."
      | Null as v -> Result.ok v
      | Array a when Array.length a < min ->
        Result.error
          ("#Slot did not reach the minimum amount of nodes (specified as "
          ^ string_of_int min
          ^ ").")
      | Array a when Array.length a < min ->
        Result.error
          ("#Slot did not reach the minimum amount of nodes (specified as "
          ^ string_of_int min
          ^ ").")
      | Array a when Array.length a > max ->
        Result.error
          ("#Slot includes more than the maximum amount of nodes (specified as "
          ^ string_of_int max
          ^ ").")
      | Array a ->
        let check_instance_restriction tag =
          match constraints with
          | None -> Result.ok ()
          | Some restrictions ->
            let is_in_list = ref false in
            let allowed, disallowed =
              restrictions
              |> Array.to_list
              |> List.partition_map (fun (name, _typ, negated) ->
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
              Result.error
                ("Child with tag `"
                ^ tag
                ^ "` may not be used inside this #Slot . The following restrictions are \
                   set: [ "
                ^ (constraints
                  |> Option.value ~default:[||]
                  |> Array.to_list
                  |> List.map (fun (name, _typ, negated) ->
                         match negated with
                         | `Negated -> "!" ^ name
                         | `NotNegated -> name)
                  |> String.concat ",")
                ^ " ]")
            else Result.ok ()
        in
        let passed, failed =
          a
          |> Array.to_list
          |> List.partition_map (function
                 | (HtmlTemplateNode (tag, _, _, _) | ComponentTemplateNode (_, tag, _, _))
                   as v ->
                   (match check_instance_restriction tag with
                   | Ok () -> Either.left v
                   | Error e -> Either.right e)
                 | _ ->
                   Either.right
                     "Tried to assign a non node value to a #Slot. Only nodes template \
                      nodes are allowed inside slots. If you want to put another value \
                      (like a string) into a slot, you have to wrap it in some node.")
        in
        (match failed with
        | [] -> Array (passed |> Array.of_list) |> Result.ok
        | hd :: _tl -> Result.error hd)
    in
    let get_value state key =
      let find_slot_key attributes =
        attributes
        |> StringMap.find_opt "slot"
        |> Option.value ~default:(String "")
        |> function
        | String s -> s
        | _ -> failwith "Expected slot attribute to be of type string"
      in
      let rec keep_slotted acc = function
        | HtmlTemplateNode (tag, attributes, children, self_closing) ->
          if find_slot_key attributes = key
          then HtmlTemplateNode (tag, attributes, children, self_closing) :: acc
          else acc
        | ComponentTemplateNode (fn, tag, attributes, result) ->
          if find_slot_key attributes = key
          then ComponentTemplateNode (fn, tag, attributes, result) :: acc
          else acc
        | Array l -> l |> Array.fold_left keep_slotted acc
        | String s when String.trim s = "" -> acc
        | _ ->
          failwith
            "Only nodes may be placed into slots. If you want to put a plain text into a \
             slot, you have to wrap it in a <p></p> tag for example."
      in
      match state.parent_component with
      | None -> failwith "#Slot is not implemented!"
      | Some (_tag, _tag_attributes, tag_children) ->
        tag_children
        |> List.fold_left keep_slotted []
        |> (function
        | [] -> None
        | list -> Some (Array (list |> List.rev |> Array.of_list)))
    in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let slot_name =
        attributes
        |> StringMap.find_opt "name"
        |> Option.value ~default:(String "")
        |> function
        | String s -> s
        | _ -> failwith "Expected attribute `name` on #Slot to be of type string."
      in
      slot_name
      |> self.get_value state
      |> Option.value ~default:(Array [||])
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let set_context =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | String _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Int _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Float _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Array _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | HtmlTemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | ComponentTemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | DefinitionInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Function _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Record _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Null -> Result.ok Null
    in
    let get_value _state _key = Some Null in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let name =
        attributes
        |> StringMap.find_opt "name"
        |> function
        | Some (String s) -> s
        | None -> failwith "attribute name is required when setting a context."
        | Some _ ->
          failwith "Expected attribute `name` on #SetContext to be of type string."
      in
      let value =
        attributes
        |> StringMap.find_opt "value"
        |> function
        | Some (Function _) -> failwith "a function can not be put into a context."
        | Some value -> value
        | None -> failwith "attribute value is required when setting a context."
      in
      Hashtbl.add state.context name value;
      name
      |> self.get_value state
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let get_context =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | ( Portal _
        | Bool _
        | String _
        | Int _
        | Float _
        | Array _
        | HtmlTemplateNode _
        | ComponentTemplateNode _
        | DefinitionInfo _
        | Function _
        | Record _
        | Null ) as v -> Result.ok v
    in
    let get_value state key = Hashtbl.find_opt state.context key in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let name =
        attributes
        |> StringMap.find_opt "name"
        |> function
        | Some (String s) -> s
        | None -> failwith "attribute name is required when getting a context."
        | Some _ ->
          failwith "Expected attribute `name` on #GetContext to be of type string."
      in
      let default = StringMap.find_opt "default" attributes in
      name
      |> self.get_value state
      |> (function
           | None | Some Null -> default
           | Some v -> Some v)
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let create_portal =
    let validate _info value =
      match value with
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | String _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Int _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Float _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Array _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Portal _ as p -> Result.ok p
      | HtmlTemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | ComponentTemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | DefinitionInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Function _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Record _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Null -> Result.error "Something unexpected happened...this is my fault, not yours"
    in
    let get_value state key = Some (Portal (Hashtbl.find_all state.portals key)) in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let name =
        attributes
        |> StringMap.find_opt "name"
        |> function
        | Some (String s) -> s
        | None -> failwith "attribute name is required when creating a portal."
        | Some _ ->
          failwith "Expected attribute `name` on #CreatePortal to be of type string."
      in
      name
      |> self.get_value state
      |> Option.value ~default:(Portal [])
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;

  let push_portal =
    let validate _info value =
      match value with
      | Portal _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | TagInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Bool _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | String _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Int _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Float _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Array _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | HtmlTemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | ComponentTemplateNode _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | DefinitionInfo _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Function _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Record _ ->
        Result.error "Something unexpected happened...this is my fault, not yours"
      | Null -> Result.ok Null
    in
    let get_value _state _key = Some Null in
    let eval ~self state info =
      let _name, _optional, attributes, _transformer = info in
      let name =
        attributes
        |> StringMap.find_opt "name"
        |> function
        | Some (String s) -> s
        | None ->
          failwith "attribute name is required when pushing a value into a portal."
        | Some _ -> failwith "Expected attribute `name` on #Portal to be of type string."
      in
      let push =
        attributes
        |> StringMap.find_opt "push"
        |> function
        | Some (Function _) -> failwith "a function can not be put into a portal."
        | Some push -> push
        | None ->
          failwith "attribute push is required when pushing a value into a portal."
      in
      let () =
        match state.mode with
        | Render -> ()
        | Portal_Collection -> Hashtbl.add state.portals name push
      in
      name
      |> self.get_value state
      |> Option.value ~default:Null
      |> self.validate info
      |> Result.map (self.transform info)
    in
    { validate; transform; eval; get_value }
  ;;
end
