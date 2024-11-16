module Value = struct
  open Types.Type_Value

  let null ?(loc = Pinc_Diagnostics.Location.none) () =
    { value_loc = loc; value_desc = Null }
  ;;

  let char ?(loc = Pinc_Diagnostics.Location.none) c =
    { value_loc = loc; value_desc = Char c }
  ;;

  let string ?(loc = Pinc_Diagnostics.Location.none) s =
    { value_loc = loc; value_desc = String s }
  ;;

  let bool ?(loc = Pinc_Diagnostics.Location.none) b =
    { value_loc = loc; value_desc = Bool b }
  ;;

  let int ?(loc = Pinc_Diagnostics.Location.none) i =
    { value_loc = loc; value_desc = Int i }
  ;;

  let float ?(loc = Pinc_Diagnostics.Location.none) f =
    { value_loc = loc; value_desc = Float f }
  ;;

  let array ?(loc = Pinc_Diagnostics.Location.none) l =
    { value_loc = loc; value_desc = Array l }
  ;;

  let list ?(loc = Pinc_Diagnostics.Location.none) l =
    { value_loc = loc; value_desc = Array (Array.of_list l) }
  ;;

  let record ?(loc = Pinc_Diagnostics.Location.none) m =
    { value_loc = loc; value_desc = Record m }
  ;;
end

module Expect = struct
  open Types.Type_Value

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
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected string, got HTML template node")
    | ComponentTemplateNode _ ->
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
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected int, got HTML template node")
    | ComponentTemplateNode _ ->
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
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected float, got HTML template node")
    | ComponentTemplateNode _ ->
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
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected bool, got HTML template node")
    | ComponentTemplateNode _ ->
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
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got HTML template node")
    | ComponentTemplateNode _ ->
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
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected array, got HTML template node")
    | ComponentTemplateNode _ ->
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
    | Record r -> Some r
    | Function _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got function definition")
    | DefinitionInfo _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got definition info")
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(error Location.none "expected record, got HTML template node")
    | ComponentTemplateNode _ ->
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
          | (`Component | `All), Some Definition_Component -> `Component
          | (`Page | `All), Some Definition_Page -> `Page
          | (`Library | `All), Some (Definition_Library _) -> `Library
          | (`Store | `All), Some (Definition_Store _) -> `Store
          | _, None ->
              Pinc_Diagnostics.(
                error Location.none ("definition \"" ^ name ^ "\" does not exist"))
          | `Component, _ ->
              Pinc_Diagnostics.(error Location.none "expected a component definition")
          | `Page, _ ->
              Pinc_Diagnostics.(error Location.none "expected a page definition")
          | `Library, _ ->
              Pinc_Diagnostics.(error Location.none "expected a library definition")
          | `Store, _ ->
              Pinc_Diagnostics.(error Location.none "expected a store definition")
        in
        Some (typ, name, negated = `Negated)
    | HtmlTemplateNode _ ->
        Pinc_Diagnostics.(
          error Location.none "expected definition info, got HTML template node")
    | ComponentTemplateNode _ ->
        Pinc_Diagnostics.(
          error Location.none "expected definition info, got component template node")
  ;;

  let constraints ~declarations ?(typ = `All) v =
    let allowed, disallowed =
      v
      |> list (required (definition_info ~typ))
      |> Option.value ~default:[]
      |> List.map (fun (_, name, negated) -> (name, negated))
      |> List.partition_map (fun (name, negated) ->
             if negated then
               Either.left name
             else
               Either.right name)
    in
    let declarations =
      StringMap.fold
        (fun id decl acc ->
          match (typ, decl.Pinc_Parser.Ast.declaration_type) with
          | (`All | `Component), Declaration_Component _ -> id :: acc
          | (`All | `Library), Declaration_Library _ -> id :: acc
          | (`All | `Page), Declaration_Page _ -> id :: acc
          | (`All | `Store), Declaration_Store _ -> id :: acc
          | _ -> acc)
        declarations
        []
    in

    let result =
      match (allowed, disallowed) with
      | [], [] -> declarations
      | [], disallowed ->
          declarations |> List.filter (fun id -> List.mem id disallowed |> not)
      | allowed, _ -> declarations |> List.filter (fun id -> List.mem id allowed)
    in

    Some result
  ;;
end

module TagMeta = struct
  open Types.Type_Tag

  let string s : meta = `String s
  let int i : meta = `Int i
  let float f : meta = `Float f
  let boolean b : meta = `Boolean b
  let array a : meta = `Array a
  let record r : meta = `Record r
  let children () : meta = `SubTagPlaceholder
  let template () : meta = `TemplatePlaceholder
  let errors ?(list = []) () : meta = `Errors list

  let rec merge (a : meta) (b : meta) =
    match (a, b) with
    | `Record a, `Record b ->
        `Record
          (List.fold_left
             (fun acc (key, entry) ->
               match List.assoc_opt key acc with
               | None -> (key, entry) :: acc
               | Some v -> (key, merge v entry) :: acc)
             a
             b)
    | `Array a, `Array b -> `Array (a @ b)
    | _a, b -> b
  ;;

  let rec map : (meta -> 'a) -> meta -> 'a =
   fun fn meta ->
    match meta with
    | `Record a -> `Record (a |> List.map @@ fun (key, v) -> (key, map fn v))
    | `Array a -> `Array (a |> List.map (map fn))
    | value -> fn value
  ;;

  let rec filter_map : (meta -> 'a option) -> meta -> 'a option =
   fun fn (meta : meta) ->
    match meta with
    | `Record a -> (
        let result =
          a
          |> List.filter_map @@ fun (key, v) ->
             filter_map fn v |> Option.map (fun v -> (key, v))
        in
        match result with
        | [] -> None
        | l -> Some (`Record l))
    | `Array a -> (
        match a |> List.filter_map (filter_map fn) with
        | [] -> None
        | l -> Some (`Array l))
    | value -> fn value
  ;;
end

let noop_data_provider ~tag:_ ~attributes:_ ~required:_ ~key:_ = None
let noop_meta_provider ~tag:_ ~attributes:_ ~required:_ ~key:_ = None
