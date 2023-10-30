open Pinc_Interpreter_Types

module Expect = struct
  let required fn value =
    match fn value with
    | None -> Pinc_Diagnostics.(error Location.none "required a value, but got null")
    | Some v -> v
  ;;

  let maybe fn value =
    match fn value with
    | exception _ -> None
    | None -> None
    | Some _ as v -> v
  ;;

  let attribute key fn value = Option.bind (StringMap.find_opt key value) fn
  let any_value v = v

  let string v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(error Location.none "expected string, got portal value")
    | String s -> Some s
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected string, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected string, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected string, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected string, got bool")
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected string, got array")
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected string, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected string, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected string, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected string, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected string, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected string, got component template node")
  ;;

  let int v =
    match v.value_desc with
    | Null -> None
    | Portal _ -> Pinc_Diagnostics.(error Location.none "expected int, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected int, got string")
    | Int i -> Some i
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected int, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected int, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected int, got bool")
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected int, got array")
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected int, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected int, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected int, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected int, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected int, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected int, got component template node")
  ;;

  let float v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(error Location.none "expected float, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected float, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected float, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected float, got char")
    | Float f -> Some f
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected float, got bool")
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected float, got array")
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected float, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected float, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected float, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected float, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected float, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected float, got component template node")
  ;;

  let bool v =
    match v.value_desc with
    | Null -> None
    | Portal _ -> Pinc_Diagnostics.(error Location.none "expected bool, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected bool, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected bool, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected bool, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected bool, got float")
    | Bool b -> Some b
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected bool, got array")
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected bool, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected bool, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected bool, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected bool, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected bool, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected bool, got component template node")
  ;;

  let array fn v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected array, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected array, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected array, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected array, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected array, bool")
    | Array a -> Some (a |> Array.map fn)
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected array, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected array, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected array, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected array, got component template node")
  ;;

  let list fn v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected array, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected array, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected array, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected array, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected array, bool")
    | Array a -> Some (a |> Array.to_list |> List.map fn)
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected array, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected array, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected array, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected array, got component template node")
  ;;

  let record v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected record, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected record, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected record, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected record, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected record, got bool")
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected record, got array")
    | Record r -> Some (r |> StringMap.map snd)
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected record, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected record, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected record, got component template node")
  ;;

  let record_with_order v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected record, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected record, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected record, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected record, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected record, got bool")
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected record, got array")
    | Record r -> r |> Option.some
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got definition info")
    | TagInfo _ -> Pinc_Diagnostics.(error Location.none "expected record, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected record, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected record, got component template node")
  ;;

  let definition_info ?(typ = `All) v =
    match v.value_desc with
    | Null -> None
    | Portal _ ->
        Pinc_Diagnostics.(
          error Location.none "expected definition info, got portal value")
    | String _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected definition info, got int")
    | Char _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got char")
    | Float _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got float")
    | Bool _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got bool")
    | Array _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got array")
    | Record _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got record")
    | Function _ ->
        Pinc_Diagnostics.(
          error Location.none "expected definition info, got function definition")
    | DefinitionInfo (name, def_typ, negated) ->
        let typ =
          match (typ, def_typ) with
          | (`Component | `All), Some `Component -> `Component
          | (`Page | `All), Some `Page -> `Page
          | (`Site | `All), Some `Site -> `Site
          | (`Library | `All), Some (`Library _) -> `Library
          | (`Store | `All), Some `Store -> `Store
          | _, None ->
              Pinc_Diagnostics.(
                error Location.none ("definition \"" ^ name ^ "\" does not exist"))
          | `Component, _ ->
              Pinc_Diagnostics.(error Location.none "expected a component definition")
          | `Page, _ ->
              Pinc_Diagnostics.(error Location.none "expected a page definition")
          | `Site, _ ->
              Pinc_Diagnostics.(error Location.none "expected a site definition")
          | `Library, _ ->
              Pinc_Diagnostics.(error Location.none "expected a library definition")
          | `Store, _ ->
              Pinc_Diagnostics.(error Location.none "expected a store definition")
        in
        Some (typ, name, negated = `Negated)
    | TagInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected definition info, got tag")
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected definition info, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(
          error Location.none "expected definition info, got component template node")
  ;;

  let tag_info v =
    match v.value_desc with
    | Null -> None
    | Portal _ -> Pinc_Diagnostics.(error Location.none "expected tag, got portal value")
    | String _ -> Pinc_Diagnostics.(error Location.none "expected tag, got string")
    | Int _ -> Pinc_Diagnostics.(error Location.none "expected tag, got int")
    | Char _ -> Pinc_Diagnostics.(error Location.none "expected tag, got char")
    | Float _ -> Pinc_Diagnostics.(error Location.none "expected tag, got float")
    | Bool _ -> Pinc_Diagnostics.(error Location.none "expected tag, got bool")
    | Array _ -> Pinc_Diagnostics.(error Location.none "expected tag, got array")
    | Record _ -> Pinc_Diagnostics.(error Location.none "expected tag, got record")
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected tag, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected tag, got definition info")
    | TagInfo i -> Some i
    | HtmlTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected tag, got HTML template node")
    | ComponentTemplateNode (_, _, _, _) ->
        Pinc_Diagnostics.(error Location.none "expected tag, got component template node")
  ;;
end
