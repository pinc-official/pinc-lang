open State
open Pinc_Parser.Ast
open Types.Type_Value
module Ast = Pinc_Parser.Ast

let find_path path value =
  let rec aux path value =
    match (path, value) with
    | key :: rest, { value_desc = Record r; _ } ->
        r |> StringMap.find_opt key |> Fun.flip Option.bind (aux rest)
    | key :: rest, { value_desc = Array a; _ } -> (
        try Array.get a (int_of_string key) |> aux rest
        with Failure _ | Invalid_argument _ -> None)
    | _, value -> value |> Option.some
  in
  aux path value
;;

module Utils = struct
  let apply_transformer
      ~eval_expression
      ~state
      ~(transformer : Ast.expression option)
      (value : Value.value) =
    let aux (transformer : Ast.expression) =
      let arguments = [ value ] in
      let maybe_fn = transformer |> eval_expression ~state |> State.get_output in
      match maybe_fn.value_desc with
      | Function { parameters; state = _; exec = _ }
        when List.compare_lengths parameters arguments > 0 ->
          let arguments_len = List.length arguments in
          let missing =
            parameters
            |> List.filteri (fun index _ -> index > arguments_len - 1)
            |> List.map (fun item -> "`" ^ item ^ "`")
            |> String.concat ", "
          in
          Pinc_Diagnostics.error
            transformer.expression_loc
            ("This transformer was provided too few arguments. The following parameters \
              are missing: "
            ^ missing)
      | Function { parameters; state = _; exec = _ }
        when List.compare_lengths parameters arguments < 0 ->
          Pinc_Diagnostics.error
            transformer.expression_loc
            ("This transformer only accepts "
            ^ string_of_int (List.length parameters)
            ^ " arguments, but was provided "
            ^ string_of_int (List.length arguments)
            ^ " here.")
      | Function { parameters; state = fn_state; exec } ->
          let arguments =
            List.combine parameters arguments |> List.to_seq |> StringMap.of_seq
          in
          state |> State.add_output ~output:(exec ~arguments ~state:fn_state ())
      | _ ->
          Pinc_Diagnostics.error
            transformer.expression_loc
            "Trying to assign a non function value to a transformer."
    in

    match transformer with
    | None -> value
    | Some transformer -> aux transformer |> State.get_output
  ;;
end

module Tag_String = struct
  let eval ~state ~required ~attributes t key =
    let meta = state.State.tag_meta_provider ~tag:Tag_String ~key ~attributes ~required in
    let data = state.State.tag_data_provider ~tag:Tag_String ~key ~attributes ~required in
    let output =
      data
      |> Option.map (function
             | { value_desc = String _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf
                      "Expected attribute %s to be of type string."
                      (key |> List.rev |> List.hd)))
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc ())
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Int = struct
  let eval ~state ~required ~attributes t key =
    let meta = state.State.tag_meta_provider ~tag:Tag_Int ~key ~attributes ~required in
    let data = state.State.tag_data_provider ~tag:Tag_Int ~key ~attributes ~required in
    let output =
      data
      |> Option.map (function
             | { value_desc = Int _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf
                      "Expected attribute %s to be of type int."
                      (key |> List.rev |> List.hd)))
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc ())
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Float = struct
  let eval ~state ~required ~attributes t key =
    let meta = state.State.tag_meta_provider ~tag:Tag_Float ~key ~attributes ~required in
    let data = state.State.tag_data_provider ~tag:Tag_Float ~key ~attributes ~required in
    let output =
      data
      |> Option.map (function
             | { value_desc = Float _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf
                      "Expected attribute %s to be of type float."
                      (key |> List.rev |> List.hd)))
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc ())
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Boolean = struct
  let eval ~state ~required ~attributes t key =
    let meta = state.tag_meta_provider ~tag:Tag_Boolean ~key ~attributes ~required in
    let data = state.tag_data_provider ~tag:Tag_Boolean ~key ~attributes ~required in
    let output =
      data
      |> Option.map (function
             | { value_desc = Bool _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf
                      "Expected attribute %s to be of type bool."
                      (key |> List.rev |> List.hd)))
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc ())
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Custom = struct
  let eval ~state ~required ~attributes ~name t key =
    let meta =
      state.tag_meta_provider ~tag:(Tag_Custom name) ~key ~attributes ~required
    in
    let data =
      state.tag_data_provider ~tag:(Tag_Custom name) ~key ~attributes ~required
    in
    let output = data |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc ()) in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Portal = struct
  let portals = Hashtbl.create 100

  let eval_push ~state ~required:_ ~attributes t key =
    if state.mode = `Portal_Collection then (
      let push =
        match attributes |> StringMap.find_opt "push" with
        | None ->
            Pinc_Diagnostics.error
              t.tag_loc
              "The attribute `push` is required when pushing a value into a portal."
        | Some { value_desc = Function _; value_loc } ->
            Pinc_Diagnostics.error value_loc "A function can not be put into a portal."
        | Some value -> value
      in
      Hashtbl.add portals key push);

    let output = Helpers.Value.null ~loc:t.tag_loc () in
    state |> State.add_output ~output
  ;;

  let eval_create ~state ~required:_ ~attributes:_ t key =
    let output =
      { value_desc = Portal (Hashtbl.find_all portals key); value_loc = t.tag_loc }
    in
    state |> State.add_output ~output
  ;;
end

module Tag_Context = struct
  let eval_set ~state ~required:_ ~attributes t key =
    let value =
      attributes |> StringMap.find_opt "value" |> function
      | None ->
          Pinc_Diagnostics.error
            t.tag_loc
            "attribute value is required when setting a context."
      | Some { value_desc = Function _; value_loc } ->
          Pinc_Diagnostics.error value_loc "a function can not be put into a context."
      | Some value -> value
    in

    let state = { state with context = state.context |> StringMap.add key value } in
    state |> State.add_output ~output:(Helpers.Value.null ~loc:t.tag_loc ())
  ;;

  let eval_get ~state ~required:_ ~attributes:_ t key =
    let output =
      state.context
      |> StringMap.find_opt key
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc ())
    in

    state |> State.add_output ~output
  ;;
end

module Tag_Store = struct
  let eval_body ~name ~state ~eval_expression ~value store =
    let tag_data_provider ~tag ~attributes ~required ~key =
      match tag with
      | Types.Type_Tag.Tag_Store _ -> (
          value
          |> StringMap.find_opt (key |> List.hd)
          |> Fun.flip Option.bind (find_path (key |> List.tl))
          |> function
          | None -> state.tag_data_provider ~tag ~attributes ~key ~required
          | value -> value)
      | Types.Type_Tag.Tag_Array ->
          value
          |> StringMap.find_opt (key |> List.rev |> List.hd)
          |> Fun.flip Option.bind (function
                 | { value_desc = Array a; _ } ->
                     a |> Array.length |> Helpers.Value.int |> Option.some
                 | _ -> None)
      | _ ->
          value
          |> StringMap.find_opt (key |> List.hd)
          |> Fun.flip Option.bind (find_path (key |> List.tl))
    in
    let tag_meta_provider ~tag ~attributes ~required ~key =
      match tag with
      | Types.Type_Tag.Tag_Store _ -> (
          value
          |> StringMap.find_opt (key |> List.hd)
          |> Fun.flip Option.bind (find_path (key |> List.tl))
          |> function
          | None -> state.tag_meta_provider ~tag ~attributes ~key ~required
          | _ -> None)
      | _ -> None
    in
    let state =
      State.make
        ~context:state.context
        ~mode:state.mode
        ~tag_meta:state.tag_meta
        ~tag_data_provider
        ~root_tag_data_provider:state.root_tag_data_provider
        ~tag_meta_provider
        ~root_tag_meta_provider:state.root_tag_meta_provider
        state.declarations
    in
    store |> Types.Type_Store.body |> eval_expression ~state |> State.get_output
    |> function
    | { value_desc = Record _; _ } as v -> v
    | { value_desc = _; value_loc } ->
        Pinc_Diagnostics.error
          value_loc
          (Printf.sprintf
             "The definition of store `%s` needs to be a record describing the shape and \
              type of values in this store."
             name)
  ;;

  let eval ~eval_expression ~state ~required ~attributes tag key =
    let name, store =
      match attributes |> StringMap.find_opt "id" with
      | None -> Pinc_Diagnostics.error tag.tag_loc "Attribute `id` is required on #Store."
      | Some
          {
            value_desc = DefinitionInfo (name, Some (Definition_Store store), `NotNegated);
            _;
          } -> (name, store)
      | Some
          {
            value_loc;
            value_desc = DefinitionInfo (_, Some (Definition_Store _), `Negated);
            _;
          } -> Pinc_Diagnostics.error value_loc "Expected store id to not be negated."
      | Some { value_loc; _ } ->
          Pinc_Diagnostics.error
            value_loc
            "Expected attribute `id` to be a Store definition."
    in
    let is_singleton = store |> Types.Type_Store.is_singleton in

    let meta =
      state.State.tag_meta_provider ~tag:(Tag_Store store) ~key ~attributes ~required
    in
    let data =
      state.State.tag_data_provider ~tag:(Tag_Store store) ~key ~attributes ~required
    in
    let output =
      match data with
      | None -> Helpers.Value.null ~loc:tag.tag_loc ()
      | Some { value_desc = Record value; _ } when is_singleton -> (
          store |> eval_body ~name ~value ~eval_expression ~state |> function
          | { value_desc = Record _; _ } as v -> v
          | { value_desc = _; value_loc } ->
              Pinc_Diagnostics.error
                value_loc
                (Printf.sprintf
                   "The definition of store `%s` needs to be a record describing the \
                    shape and type of values in this store."
                   name))
      | Some { value_desc = _; value_loc } when is_singleton ->
          Pinc_Diagnostics.error
            value_loc
            (Printf.sprintf
               "Expected attribute %s to be a record."
               (key |> List.rev |> List.hd))
      | Some { value_desc = Array array; _ } ->
          array
          |> Array.map (function
                 | { value_desc = Record value; _ } ->
                     store |> eval_body ~name ~value ~eval_expression ~state
                 | { value_desc = _; value_loc } ->
                     Pinc_Diagnostics.error
                       value_loc
                       (Printf.sprintf
                          "Expected attribute %s to be an array of records."
                          (key |> List.rev |> List.hd)))
          |> Helpers.Value.array ~loc:tag.tag_loc
      | Some { value_desc = _; value_loc } ->
          Pinc_Diagnostics.error
            value_loc
            (Printf.sprintf
               "Expected attribute %s to be an array."
               (key |> List.rev |> List.hd))
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Slot = struct
  let find_slot_key attributes =
    attributes
    |> StringMap.find_opt "slot"
    |> Option.value ~default:(Helpers.Value.string "")
    |> function
    | { value_desc = String s; _ } -> s
    | { value_loc; _ } ->
        Pinc_Diagnostics.error value_loc "Expected slot attribute to be of type string"
  ;;

  let rec keep_slotted ~key acc el =
    match el with
    | ( { value_desc = HtmlTemplateNode (_, attributes, _, _); _ }
      | { value_desc = ComponentTemplateNode (_, attributes, _); _ } ) as v ->
        if find_slot_key attributes = key then
          v :: acc
        else
          acc
    | { value_desc = Array l; _ } -> l |> Array.fold_left (keep_slotted ~key) acc
    | { value_desc = String s; _ } when String.trim s = "" -> acc
    | { value_loc; _ } ->
        Pinc_Diagnostics.error
          value_loc
          "Only template nodes are allowed inside slots. If you want to put another \
           value (like a string) into a slot, you have to wrap it in some html tag or \
           component."
  ;;

  let check_instance_restriction ~constraints tag =
    match constraints with
    | None -> Result.ok ()
    | Some [||] ->
        Result.error
          (Printf.sprintf
             "Child with tag `%s` may not be used inside this #Slot. \n\
              It has an empty array set as constrints, which leads to nothing being \
              allowed to be placed inside."
             tag)
    | Some restrictions ->
        let is_in_list = ref false in
        let allowed, disallowed =
          restrictions
          |> Array.to_list
          |> List.partition_map (fun (name, negated) ->
                 if name = tag then
                   is_in_list := true;
                 if negated = `Negated then
                   Either.right name
                 else
                   Either.left name)
        in
        let is_allowed =
          match (allowed, disallowed) with
          | [], _disallowed -> not !is_in_list
          | _allowed, [] -> !is_in_list
          | allowed, _disallowed -> List.mem tag allowed
        in
        if is_allowed then
          Result.ok ()
        else (
          let contraints =
            constraints
            |> Option.map Array.to_list
            |> Option.value ~default:[]
            |> List.map (fun (name, negated) ->
                   if negated = `Negated then
                     "!" ^ name
                   else
                     name)
            |> String.concat ","
          in
          Result.error
            (Printf.sprintf
               "Child with tag `%s` may not be used inside this #Slot. The following \
                restrictions are set: [ %s ]"
               tag
               contraints))
  ;;

  let validate_element_count ~attributes slotted_elements =
    let min =
      attributes
      |> StringMap.find_opt "min"
      |> Option.map (function
             | { value_desc = Int i; _ } -> i
             | { value_loc; _ } ->
                 Pinc_Diagnostics.error
                   value_loc
                   "Expected attribute min to be of type int.")
      |> Option.value ~default:0
    in

    let max =
      attributes
      |> StringMap.find_opt "max"
      |> Option.map (function
             | { value_desc = Int i; _ } -> i
             | { value_loc; _ } ->
                 Pinc_Diagnostics.error
                   value_loc
                   "Expected attribute max to be of type int.")
      |> Option.value ~default:Int.max_int
    in
    let num_slotted_elements = Array.length slotted_elements in

    match (num_slotted_elements < min, num_slotted_elements > max) with
    | true, _ ->
        Result.error
        @@ Printf.sprintf
             "This #Slot did not reach the minimum amount of nodes (specified as %i)."
             min
    | _, true ->
        Result.error
        @@ Printf.sprintf
             "This #Slot was provided more than the maximum amount of nodes (specified \
              as %i)."
             max
    | false, false -> Result.ok ()
  ;;

  let eval ~eval_expression ~state ~required ~attributes tag_value key =
    let constraints =
      attributes
      |> StringMap.find_opt "constraints"
      |> (Option.map @@ function
          | { value_desc = Array a; _ } -> a
          | { value_desc = _; value_loc } ->
              Pinc_Diagnostics.error
                value_loc
                "slot contraints need to be an array of definitions which are either \
                 allowed or disallowed")
      |> Option.map
         @@ Array.map
         @@ function
         | { value_desc = DefinitionInfo (name, Some Definition_Component, negated); _ }
           -> (name, negated)
         | { value_desc = DefinitionInfo (name, None, _negated); value_loc } ->
             Pinc_Diagnostics.error
               value_loc
               (Printf.sprintf "definition `%s` does not exist" name)
         | { value_desc = DefinitionInfo (name, _typ, _negated); value_loc } ->
             Pinc_Diagnostics.error
               value_loc
               (Printf.sprintf
                  "definition `%s` is not a component. Expected to see a component \
                   definition at this point."
                  name)
         | { value_desc = _; value_loc } ->
             Pinc_Diagnostics.error
               value_loc
               "Expected to see a component definition at this point"
    in

    let tag =
      Types.Type_Tag.Tag_Slot
        (fun ~tag ~tag_data_provider ->
          let state =
            State.make
              ~context:state.context
              ~mode:state.mode
              ~tag_meta:state.tag_meta
              ~root_tag_data_provider:state.root_tag_data_provider
              ~tag_data_provider
              ~root_tag_meta_provider:state.root_tag_meta_provider
              ~tag_meta_provider:state.root_tag_meta_provider
              state.declarations
          in

          let evaluated =
            tag |> DeclarationEvaluator.eval ~eval_expression ~state |> State.get_output
          in

          {
            value_loc = Pinc_Diagnostics.Location.none;
            value_desc = Value.ComponentTemplateNode (tag, StringMap.empty, evaluated);
          })
    in

    let meta = state.State.tag_meta_provider ~tag ~key ~attributes ~required in
    let data = state.State.tag_data_provider ~tag ~key ~attributes ~required in

    let slotted_elements =
      match data with
      | None -> [||]
      | Some { value_desc = Array a; _ } -> a
      | _ ->
          Pinc_Diagnostics.error
            tag_value.tag_loc
            (Printf.sprintf
               "Expected attribute %s to be an array of elements."
               (key |> List.rev |> List.hd))
    in

    let () =
      validate_element_count ~attributes slotted_elements
      |> Result.iter_error (Pinc_Diagnostics.error tag_value.tag_loc)
    in

    let () =
      slotted_elements
      |> Array.iter @@ function
         | { value_desc = HtmlTemplateNode (tag_name, _, _, _); value_loc }
         | { value_desc = ComponentTemplateNode (tag_name, _, _); value_loc } ->
             check_instance_restriction ~constraints tag_name
             |> Result.iter_error (Pinc_Diagnostics.error value_loc)
         | { value_desc = _; value_loc } ->
             Pinc_Diagnostics.error
               value_loc
               "Tried to assign a non node value to a #Slot. Only template nodes are \
                allowed inside slots. If you want to put another value (like a string) \
                into a slot, you have to wrap it in some html tag or component."
    in

    let output = slotted_elements |> Helpers.Value.array ~loc:tag_value.tag_loc in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Record = struct
  let eval ~eval_expression ~state ~required ~attributes ~of' t key =
    let meta = state.State.tag_meta_provider ~tag:Tag_Record ~key ~attributes ~required in
    let data = state.State.tag_data_provider ~tag:Tag_Record ~key ~attributes ~required in
    let output, child_meta =
      data
      |> Option.map (function
             | { value_desc = Record r; _ } -> r
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf
                      "Expected attribute %s to be a record."
                      (key |> List.rev |> List.hd)))
      |> Option.map (fun _ ->
             let state = { state with tag_path = key; tag_meta = [] } in
             match of' with
             | None ->
                 Pinc_Diagnostics.error t.tag_loc "Attribute `of` is required on #Record."
             | Some children ->
                 let state = children |> eval_expression ~state in
                 let meta = state.tag_meta in
                 let value =
                   state |> State.get_output |> function
                   | { value_desc = Record _; _ } as v -> v
                   | { value_desc = _; value_loc } ->
                       Pinc_Diagnostics.error
                         value_loc
                         "Attribute `of` needs to be a record describing the shape and \
                          type of values in this record."
                 in
                 (value, meta))
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc (), [])
    in

    let meta =
      meta
      |> Option.map
           (Helpers.TagMeta.map @@ function
            | `SubTagPlaceholder -> child_meta |> Helpers.TagMeta.record
            | v -> v)
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

module Tag_Array = struct
  let eval ~eval_expression ~state ~required ~attributes ~of' t key =
    let meta = state.State.tag_meta_provider ~tag:Tag_Array ~key ~attributes ~required in
    let data = state.State.tag_data_provider ~tag:Tag_Array ~key ~attributes ~required in
    let output, child_meta, template_meta =
      data
      |> Option.map (function
             | { value_desc = Int i; _ } -> i
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf
                      "Expected attribute %s to be an int (the length of the array)."
                      (key |> List.rev |> List.hd)))
      |> Option.map (fun len ->
             match of' with
             | None ->
                 Pinc_Diagnostics.error t.tag_loc "Attribute `of` is required on #Array."
             | Some children ->
                 let state = { state with tag_path = key; tag_meta = [] } in
                 let template_meta =
                   let state =
                     children
                     |> eval_expression
                          ~state:{ state with binding_identifier = Some (`Optional, "0") }
                   in
                   match state.tag_meta with
                   | (_, meta) :: _ -> Some meta
                   | _ -> None
                 in
                 let values, metas =
                   List.init len (fun i ->
                       let binding_identifier = Some (`Required, string_of_int i) in
                       let state =
                         children
                         |> eval_expression ~state:{ state with binding_identifier }
                       in
                       let value = state |> State.get_output in
                       let meta =
                         match state.tag_meta with
                         | (_, meta) :: _ -> Some meta
                         | _ -> None
                       in
                       (value, meta))
                   |> List.split
                 in
                 let value = values |> Helpers.Value.list ~loc:t.tag_loc in
                 let meta = metas |> List.filter_map Fun.id in
                 (value, meta, template_meta))
      |> Option.value ~default:(Helpers.Value.null ~loc:t.tag_loc (), [], None)
    in

    let meta =
      meta
      |> Fun.flip Option.bind
         @@ Helpers.TagMeta.filter_map
         @@ function
         | `SubTagPlaceholder -> child_meta |> Helpers.TagMeta.array |> Option.some
         | `TemplatePlaceholder -> template_meta
         | v -> Some v
    in

    state
    |> State.add_output ~output
    |> State.add_tag_meta ~meta (key |> String.concat ".")
  ;;
end

let eval ~eval_expression ~state t =
  let { tag; attributes; transformer } = t.tag_desc in
  let key =
    attributes
    |> StringMap.find_opt "key"
    |> Option.map (fun it -> it |> eval_expression ~state |> State.get_output)
  in
  let key, state =
    match (key, state.binding_identifier) with
    | None, Some (_optional, ident) -> (ident, { state with binding_identifier = None })
    | Some { value_desc = String key; _ }, _ -> (key, state)
    | Some { value_desc = _; value_loc }, _ ->
        Pinc_Diagnostics.error
          value_loc
          "Expected attribute `key` on tag to be of type string"
    | None, None -> ("", state)
  in
  let path = state.tag_path @ [ key ] in
  let of' = attributes |> StringMap.find_opt "of" in
  let attributes =
    attributes
    |> StringMap.remove "of"
    |> StringMap.map (fun it -> it |> eval_expression ~state |> State.get_output)
  in

  let required =
    state.binding_identifier
    |> (Option.map @@ fun v -> fst v = `Required)
    |> Option.value ~default:true
  in

  let state =
    match tag with
    | Tag_SetContext -> key |> Tag_Context.eval_set ~state ~required ~attributes t
    | Tag_GetContext -> key |> Tag_Context.eval_get ~state ~required ~attributes t
    | Tag_CreatePortal -> key |> Tag_Portal.eval_create ~state ~required ~attributes t
    | Tag_Portal -> key |> Tag_Portal.eval_push ~state ~required ~attributes t
    | Tag_Slot -> path |> Tag_Slot.eval ~eval_expression ~state ~required ~attributes t
    | Tag_Store -> path |> Tag_Store.eval ~eval_expression ~state ~required ~attributes t
    | Tag_String -> path |> Tag_String.eval ~state ~required ~attributes t
    | Tag_Int -> path |> Tag_Int.eval ~state ~required ~attributes t
    | Tag_Float -> path |> Tag_Float.eval ~state ~required ~attributes t
    | Tag_Boolean -> path |> Tag_Boolean.eval ~state ~required ~attributes t
    | Tag_Array ->
        path |> Tag_Array.eval ~eval_expression ~state ~required ~attributes ~of' t
    | Tag_Record ->
        path |> Tag_Record.eval ~eval_expression ~state ~required ~attributes ~of' t
    | Tag_Custom name -> path |> Tag_Custom.eval ~state ~required ~attributes ~name t
  in

  let transformed_value =
    state
    |> State.get_output
    |> Utils.apply_transformer ~eval_expression ~state ~transformer
  in

  state |> State.add_output ~output:transformed_value
;;
