let ( let* ) = Result.bind

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
    | Ok None -> Result.error `Required
    | Ok (Some v) -> Result.ok v
    | Error _ as e -> e
  ;;

  let maybe fn value =
    match fn value with
    | Error _ -> None
    | Ok None -> None
    | Ok (Some _ as v) -> v
  ;;

  let attribute key fn value =
    let value = StringMap.find_opt key value in
    match value with
    | None -> Ok None
    | Some v -> fn v
  ;;

  let any_value v = Ok v

  let string v =
    match v.value_desc with
    | Null -> Result.ok None
    | String s -> Result.ok (Some s)
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let int v =
    match v.value_desc with
    | Null -> Result.ok None
    | Int i -> Result.ok (Some i)
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let float v =
    match v.value_desc with
    | Null -> Result.ok None
    | Float f -> Result.ok (Some f)
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let bool v =
    match v.value_desc with
    | Null -> Result.ok None
    | Bool b -> Result.ok (Some b)
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let list fn v =
    match v.value_desc with
    | Null -> Result.ok None
    | Array a ->
        Array.fold_left
          (fun acc x ->
            match (fn x, acc) with
            | Error e, _ | _, Error e -> Error e
            | Ok v, Ok acc -> Ok (v :: acc))
          (Ok [])
          a
        |> Result.map (fun l -> l |> List.rev |> Option.some)
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let array fn v = list fn v |> Result.map (Option.map Array.of_list)

  let record v =
    match v.value_desc with
    | Null -> Result.ok None
    | Record r -> Result.ok (Some r)
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let definition_info ?(typ = `All) v =
    match v.value_desc with
    | Null -> Result.ok None
    | DefinitionInfo (name, def_typ, negated) ->
        let* typ =
          match (typ, def_typ) with
          | (`Component | `All), Some Definition_Component -> Ok `Component
          | (`Page | `All), Some Definition_Page -> Ok `Page
          | (`Library | `All), Some (Definition_Library _) -> Ok `Library
          | (`Store | `All), Some (Definition_Store _) -> Ok `Store
          | _, None -> Result.error (`MissingDefinition v.value_loc)
          | `Component, _ -> Result.error (`UnexpectedType v.value_loc)
          | `Page, _ -> Result.error (`UnexpectedType v.value_loc)
          | `Library, _ -> Result.error (`UnexpectedType v.value_loc)
          | `Store, _ -> Result.error (`UnexpectedType v.value_loc)
        in
        Result.ok (Some (typ, name, negated = `Negated))
    | _ -> Result.error (`UnexpectedType v.value_loc)
  ;;

  let constraints ~declarations ?(typ = `All) v =
    let* l = v |> list (required (definition_info ~typ)) in
    let disallowed, allowed =
      l
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
          declarations |> List.filter (fun id -> not @@ List.mem id disallowed)
      | allowed, _ -> declarations |> List.filter (fun id -> List.mem id allowed)
    in

    Result.ok @@ Some result
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
