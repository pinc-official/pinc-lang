type t = StringSet.t StringMap.t

let rec collect_expr acc (expr : Pinc_Types.Ast.expression) =
  match expr.expression_desc with
  | Void
  | String _
  | Char _
  | Int _
  | Float _
  | Bool _
  | LowercaseIdentifierExpression _
  | ExternalFunction _ -> acc
  | UppercaseIdentifierExpression name -> StringSet.add name acc
  | Array elems -> Array.fold_left collect_expr acc elems
  | Record fields -> StringMap.fold (fun _ (_, e) a -> collect_expr a e) fields acc
  | Function { body; _ } -> collect_expr acc body
  | FunctionCall { function_definition; arguments } ->
      let acc = collect_expr acc function_definition in
      List.fold_left collect_expr acc arguments
  | TagExpression tag -> collect_tag acc tag
  | ForInExpression { iterable; body; _ } ->
      let acc = collect_expr acc iterable in
      collect_expr acc body
  | TemplateExpression node -> collect_template_node acc node
  | BlockExpression stmts -> List.fold_left collect_stmt acc stmts
  | ConditionalExpression { condition; consequent; alternate } -> (
      let acc = collect_expr acc condition in
      let acc = collect_expr acc consequent in
      match alternate with
      | None -> acc
      | Some s -> collect_expr acc s)
  | UnaryExpression (_, e) -> collect_expr acc e
  | BinaryExpression (l, _, r) ->
      let acc = collect_expr acc l in
      collect_expr acc r

and collect_stmt acc (stmt : Pinc_Types.Ast.statement) =
  match stmt.statement_desc with
  | BreakStatement _ | ContinueStatement _ -> acc
  | LetStatement (_, e, ..) | MutationStatement (_, e) | ExpressionStatement e ->
      collect_expr acc e

and collect_tag acc (tag : Pinc_Types.Ast.tag) =
  let acc = StringMap.fold (fun _ e a -> collect_expr a e) tag.tag_desc.attributes acc in
  let acc =
    match tag.tag_desc.transformer with
    | None -> acc
    | Some e -> collect_expr acc e
  in
  match tag.tag_desc.children with
  | None -> acc
  | Some e -> collect_expr acc e

and collect_template_node acc (node : Pinc_Types.Ast.template_node) =
  match node.template_node_desc with
  | TextTemplateNode _ -> acc
  | FragmentTemplateNode fragment_children ->
      List.fold_left collect_template_node acc fragment_children
  | ExpressionTemplateNode e -> collect_expr acc e
  | HtmlTemplateNode { html_tag_attributes; html_tag_children; _ } ->
      let acc = StringMap.fold (fun _ e a -> collect_expr a e) html_tag_attributes acc in
      List.fold_left collect_template_node acc html_tag_children
  | ComponentTemplateNode
      {
        component_tag_identifier = Uppercase_Id (name, _);
        component_tag_attributes;
        component_tag_children;
      } ->
      let acc = StringSet.add name acc in
      let acc =
        StringMap.fold (fun _ e a -> collect_expr a e) component_tag_attributes acc
      in
      List.fold_left collect_template_node acc component_tag_children
;;

let declaration_deps (decl : Pinc_Types.Ast.declaration) : StringSet.t =
  let acc =
    StringMap.fold
      (fun _ e a -> collect_expr a e)
      decl.declaration_attributes
      StringSet.empty
  in
  collect_expr acc decl.declaration_body
;;

let cyclic_dependencies (graph : t) =
  let resolve_status = Hashtbl.create (StringMap.cardinal graph) in

  let rec aux key dependencies resolved_as_circular path =
    Hashtbl.replace resolve_status key `Unresolved;

    let result =
      StringSet.fold
        (fun dependency resolved_as_circular ->
          let path = dependency :: path in
          match Hashtbl.find_opt resolve_status dependency with
          | Some `Resolved -> resolved_as_circular
          | Some `Unresolved -> (key, List.rev path) :: resolved_as_circular
          | None -> (
              match StringMap.find_opt dependency graph with
              | None -> resolved_as_circular
              | Some dependencies -> aux dependency dependencies resolved_as_circular path
              ))
        dependencies
        resolved_as_circular
    in

    Hashtbl.replace resolve_status key `Resolved;
    result
  in

  StringMap.fold (fun key dependencies acc -> aux key dependencies acc [ key ]) graph []
;;

let report_cyclic_dependencies ast deps =
  List.iter
    (fun (key, path) ->
      let declaration = StringMap.find key ast in
      let path = String.concat " -> " path in
      Pinc_Diagnostics.raise_error
        declaration.Pinc_Types.Ast.declaration_loc
        (Printf.sprintf "Found cyclic dependency in `%s`:\n%s\n%!" key path))
    deps
;;

let build (ast : Pinc_Types.Ast.t) : t =
  let graph = StringMap.map declaration_deps ast in
  let () = report_cyclic_dependencies ast @@ cyclic_dependencies graph in
  graph
;;

let dependencies_of (graph : t) (name : string) : StringSet.t =
  StringMap.find_opt name graph |> Option.value ~default:StringSet.empty
;;

let transitive_dependencies_of (graph : t) (name : string) : StringSet.t =
  let rec aux acc name =
    let direct = dependencies_of graph name in

    StringSet.fold
      (fun dep acc ->
        if StringSet.mem dep acc then
          acc
        else (
          let acc = StringSet.add dep acc in
          StringSet.union acc (aux acc dep)))
      direct
      acc
  in

  aux StringSet.empty name
;;
