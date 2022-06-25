open Pinc_Interpreter_Types

module Expect = struct
  let required fn value =
    match fn value with
    | None ->
        failwith "required a value, but got null"
    | Some v ->
        v

  let attribute key fn value = Option.bind (StringMap.find_opt key value) fn

  let string = function
    | Null ->
        None
    | Portal _ ->
        failwith "expected string, got portal value"
    | String s ->
        Some s
    | Int _ ->
        failwith "expected string, got int"
    | Float _ ->
        failwith "expected string, got float"
    | Bool _ ->
        failwith "expected string, got bool"
    | Array _ ->
        failwith "expected string, got array"
    | Record _ ->
        failwith "expected string, got record"
    | Function _ ->
        failwith "expected string, got function definition"
    | DefinitionInfo _ ->
        failwith "expected string, got definition info"
    | TagInfo _ ->
        failwith "expected string, got tag"
    | HtmlTemplateNode (_, _, _, _) ->
        failwith "expected string, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected string, got component template node"

  let int = function
    | Null ->
        None
    | Portal _ ->
        failwith "expected int, got portal value"
    | String _ ->
        failwith "expected int, got string"
    | Int i ->
        Some i
    | Float _ ->
        failwith "expected int, got float"
    | Bool _ ->
        failwith "expected int, got bool"
    | Array _ ->
        failwith "expected int, got array"
    | Record _ ->
        failwith "expected int, got record"
    | Function _ ->
        failwith "expected int, got function definition"
    | DefinitionInfo _ ->
        failwith "expected int, got definition info"
    | TagInfo _ ->
        failwith "expected int, got tag"
    | HtmlTemplateNode (_, _, _, _) ->
        failwith "expected int, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected int, got component template node"

  let array fn = function
    | Null ->
        None
    | Portal _ ->
        failwith "expected array, got portal value"
    | String _ ->
        failwith "expected array, got string"
    | Int _ ->
        failwith "expected array, got int"
    | Float _ ->
        failwith "expected array, got float"
    | Bool _ ->
        failwith "expected array, bool"
    | Array a ->
        Some (a |> Array.to_list |> List.map fn)
    | Record _ ->
        failwith "expected array, got record"
    | Function _ ->
        failwith "expected array, got function definition"
    | DefinitionInfo _ ->
        failwith "expected array, got definition info"
    | TagInfo _ ->
        failwith "expected array, got tag"
    | HtmlTemplateNode (_, _, _, _) ->
        failwith "expected array, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected array, got component template node"

  let record = function
    | Null ->
        None
    | Portal _ ->
        failwith "expected record, got portal value"
    | String _ ->
        failwith "expected record, got string"
    | Int _ ->
        failwith "expected record, got int"
    | Float _ ->
        failwith "expected record, got float"
    | Bool _ ->
        failwith "expected record, got bool"
    | Array _ ->
        failwith "expected record, got array"
    | Record r ->
        Some r
    | Function _ ->
        failwith "expected record, got function definition"
    | DefinitionInfo _ ->
        failwith "expected record, got definition info"
    | TagInfo _ ->
        failwith "expected record, got tag"
    | HtmlTemplateNode (_, _, _, _) ->
        failwith "expected record, got HTML template node"
    | ComponentTemplateNode (_, _, _, _) ->
        failwith "expected record, got component template node"
end