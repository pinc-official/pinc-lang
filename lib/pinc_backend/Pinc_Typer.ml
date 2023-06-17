open Pinc_Interpreter_Types

module Expect = struct
  let required fn value =
    match fn value with
    | None -> failwith "required a value, but got null"
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

  let string = function
    | Null -> None
    | Portal _ -> failwith "expected string, got portal value"
    | String s -> Some s
    | Int _ -> failwith "expected string, got int"
    | Float _ -> failwith "expected string, got float"
    | Bool _ -> failwith "expected string, got bool"
    | Array _ -> failwith "expected string, got array"
    | Record _ -> failwith "expected string, got record"
    | Function _ -> failwith "expected string, got function definition"
    | DefinitionInfo _ -> failwith "expected string, got definition info"
    | TagInfo _ -> failwith "expected string, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected string, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected string, got component template node"
  ;;

  let int = function
    | Null -> None
    | Portal _ -> failwith "expected int, got portal value"
    | String _ -> failwith "expected int, got string"
    | Int i -> Some i
    | Float _ -> failwith "expected int, got float"
    | Bool _ -> failwith "expected int, got bool"
    | Array _ -> failwith "expected int, got array"
    | Record _ -> failwith "expected int, got record"
    | Function _ -> failwith "expected int, got function definition"
    | DefinitionInfo _ -> failwith "expected int, got definition info"
    | TagInfo _ -> failwith "expected int, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected int, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected int, got component template node"
  ;;

  let float = function
    | Null -> None
    | Portal _ -> failwith "expected float, got portal value"
    | String _ -> failwith "expected float, got string"
    | Int _ -> failwith "expected float, got int"
    | Float f -> Some f
    | Bool _ -> failwith "expected float, got bool"
    | Array _ -> failwith "expected float, got array"
    | Record _ -> failwith "expected float, got record"
    | Function _ -> failwith "expected float, got function definition"
    | DefinitionInfo _ -> failwith "expected float, got definition info"
    | TagInfo _ -> failwith "expected float, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected float, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected float, got component template node"
  ;;

  let bool = function
    | Null -> None
    | Portal _ -> failwith "expected bool, got portal value"
    | String _ -> failwith "expected bool, got string"
    | Int _ -> failwith "expected bool, got int"
    | Float _ -> failwith "expected bool, got float"
    | Bool b -> Some b
    | Array _ -> failwith "expected bool, got array"
    | Record _ -> failwith "expected bool, got record"
    | Function _ -> failwith "expected bool, got function definition"
    | DefinitionInfo _ -> failwith "expected bool, got definition info"
    | TagInfo _ -> failwith "expected bool, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected bool, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected bool, got component template node"
  ;;

  let array fn = function
    | Null -> None
    | Portal _ -> failwith "expected array, got portal value"
    | String _ -> failwith "expected array, got string"
    | Int _ -> failwith "expected array, got int"
    | Float _ -> failwith "expected array, got float"
    | Bool _ -> failwith "expected array, bool"
    | Array a -> Some (a |> Array.to_list |> List.map fn)
    | Record _ -> failwith "expected array, got record"
    | Function _ -> failwith "expected array, got function definition"
    | DefinitionInfo _ -> failwith "expected array, got definition info"
    | TagInfo _ -> failwith "expected array, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected array, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected array, got component template node"
  ;;

  let record = function
    | Null -> None
    | Portal _ -> failwith "expected record, got portal value"
    | String _ -> failwith "expected record, got string"
    | Int _ -> failwith "expected record, got int"
    | Float _ -> failwith "expected record, got float"
    | Bool _ -> failwith "expected record, got bool"
    | Array _ -> failwith "expected record, got array"
    | Record r -> Some (r |> StringMap.map snd)
    | Function _ -> failwith "expected record, got function definition"
    | DefinitionInfo _ -> failwith "expected record, got definition info"
    | TagInfo _ -> failwith "expected record, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected record, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected record, got component template node"
  ;;

  let record_with_order = function
    | Null -> None
    | Portal _ -> failwith "expected record, got portal value"
    | String _ -> failwith "expected record, got string"
    | Int _ -> failwith "expected record, got int"
    | Float _ -> failwith "expected record, got float"
    | Bool _ -> failwith "expected record, got bool"
    | Array _ -> failwith "expected record, got array"
    | Record r -> Some r
    | Function _ -> failwith "expected record, got function definition"
    | DefinitionInfo _ -> failwith "expected record, got definition info"
    | TagInfo _ -> failwith "expected record, got tag"
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected record, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected record, got component template node"
  ;;

  let definition_info ?(typ = `All) = function
    | Null -> None
    | Portal _ -> failwith "expected definition info, got portal value"
    | String _ -> failwith "expected definition info, got string"
    | Int _ -> failwith "expected definition info, got int"
    | Float _ -> failwith "expected definition info, got float"
    | Bool _ -> failwith "expected definition info, got bool"
    | Array _ -> failwith "expected definition info, got array"
    | Record _ -> failwith "expected definition info, got record"
    | Function _ -> failwith "expected definition info, got function definition"
    | DefinitionInfo (name, def_typ, negated) ->
        let typ =
          match (typ, def_typ) with
          | (`Component | `All), Some `Component -> `Component
          | (`Page | `All), Some `Page -> `Page
          | (`Site | `All), Some `Site -> `Site
          | (`Library | `All), Some (`Library _) -> `Library
          | (`Store | `All), Some `Store -> `Store
          | _, None -> failwith ("definition \"" ^ name ^ "\" does not exist")
          | `Component, _ -> failwith "expected a component definition"
          | `Page, _ -> failwith "expected a page definition"
          | `Site, _ -> failwith "expected a site definition"
          | `Library, _ -> failwith "expected a library definition"
          | `Store, _ -> failwith "expected a store definition"
        in
        Some (typ, name, negated = `Negated)
    | TagInfo _ -> failwith "expected definition info, got tag"
    | HtmlTemplateNode (_, _, _, _) ->
        failwith "expected definition info, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected definition info, got component template node"
  ;;

  let tag_info = function
    | Null -> None
    | Portal _ -> failwith "expected tag, got portal value"
    | String _ -> failwith "expected tag, got string"
    | Int _ -> failwith "expected tag, got int"
    | Float _ -> failwith "expected tag, got float"
    | Bool _ -> failwith "expected tag, got bool"
    | Array _ -> failwith "expected tag, got array"
    | Record _ -> failwith "expected tag, got record"
    | Function _ -> failwith "expected tag, got function definition"
    | DefinitionInfo _ -> failwith "expected tag, got definition info"
    | TagInfo i -> Some i
    | HtmlTemplateNode (_, _, _, _) -> failwith "expected tag, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected tag, got component template node"
  ;;
end
