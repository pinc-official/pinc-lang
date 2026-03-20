(* ------------------------------------------------------------------ *)
(* Types                                                                *)
(* ------------------------------------------------------------------ *)

(* Adjacency map: declaration name -> set of names it directly depends on *)
type t = StringSet.t StringMap.t

(* A group produced by SCC decomposition and topological sort.
   Single declarations that are not self-recursive are singletons.
   Mutually recursive declarations are grouped together. *)
type group =
  | Single of string
  | Recursive of string list

(* ------------------------------------------------------------------ *)
(* Dependency collection                                                *)
(* ------------------------------------------------------------------ *)

(* Collect all syntactic declaration-level dependencies from an expression.
   A dependency is any uppercase name that refers to another declaration:
   - UseStatement RHS
   - UppercaseIdentifierExpression
   - UppercaseIdentifierPathExpression head segment
   - ComponentTemplateNode identifier *)

let rec collect_expr acc (expr : Ast.expression) =
  match expr.expression_desc with
  | Comment _
  | String _
  | Char _
  | Int _
  | Float _
  | Bool _
  | LowercaseIdentifierExpression _
  | ExternalFunction _ -> acc
  | UppercaseIdentifierExpression name -> StringSet.add name acc
  | UppercaseIdentifierPathExpression (hd :: _) ->
      (* Only the head is a declaration name — the rest are sub-library paths *)
      StringSet.add hd acc
  | UppercaseIdentifierPathExpression [] -> acc
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
  | TemplateExpression nodes -> List.fold_left collect_template_node acc nodes
  | BlockExpression stmts -> List.fold_left collect_stmt acc stmts
  | ConditionalExpression { condition; consequent; alternate } -> (
      let acc = collect_expr acc condition in
      let acc = collect_stmt acc consequent in
      match alternate with
      | None -> acc
      | Some s -> collect_stmt acc s)
  | UnaryExpression (_, e) -> collect_expr acc e
  | BinaryExpression (l, _, r) ->
      let acc = collect_expr acc l in
      collect_expr acc r

and collect_stmt acc (stmt : Ast.statement) =
  match stmt.statement_desc with
  | CommentStatement _ | BreakStatement _ | ContinueStatement _ -> acc
  | UseStatement (_, e)
  | LetStatement (_, e)
  | MutableLetStatement (_, e)
  | OptionalLetStatement (_, e)
  | OptionalMutableLetStatement (_, e)
  | MutationStatement (_, e)
  | ExpressionStatement e -> collect_expr acc e

and collect_tag acc (tag : Ast.tag) =
  let acc = StringMap.fold (fun _ e a -> collect_expr a e) tag.tag_desc.attributes acc in
  let acc =
    match tag.tag_desc.transformer with
    | None -> acc
    | Some e -> collect_expr acc e
  in
  match tag.tag_desc.children with
  | None -> acc
  | Some e -> collect_expr acc e

and collect_template_node acc (node : Ast.template_node) =
  match node.template_node_desc with
  | TextTemplateNode _ -> acc
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

(* Collect all syntactic dependencies of a single declaration *)
let declaration_deps (decl : Ast.declaration) : StringSet.t =
  let desc =
    match decl.declaration_kind with
    | Declaration_Component d
    | Declaration_Library d
    | Declaration_Page d
    | Declaration_Store d -> d
  in
  let acc =
    StringMap.fold
      (fun _ e a -> collect_expr a e)
      desc.declaration_attributes
      StringSet.empty
  in
  collect_expr acc desc.declaration_body
;;

let build (ast : Ast.t) : t = StringMap.map declaration_deps ast

(* ------------------------------------------------------------------ *)
(* Graph queries                                                        *)
(* ------------------------------------------------------------------ *)

let dependencies_of (graph : t) (name : string) : StringSet.t =
  StringMap.find_opt name graph |> Option.value ~default:StringSet.empty
;;

let rec transitive_dependencies_of (graph : t) (name : string) : StringSet.t =
  let direct = dependencies_of graph name in
  StringSet.fold
    (fun dep acc ->
      if StringSet.mem dep acc then
        acc
        (* already visited — avoid infinite loop on cycles *)
      else (
        let acc = StringSet.add dep acc in
        StringSet.union acc (transitive_dependencies_of graph dep)))
    direct
    StringSet.empty
;;

(* ------------------------------------------------------------------ *)
(* SCC decomposition (Tarjan's algorithm)                               *)
(* ------------------------------------------------------------------ *)

(* Tarjan's algorithm produces SCCs in reverse topological order.
   We reverse at the end to get dependency order — earlier groups
   have no dependencies on later ones. *)

type tarjan_state = {
  mutable index : int;
  mutable stack : string list;
  mutable on_stack : bool StringMap.t;
  mutable indices : int StringMap.t;
  mutable lowlinks : int StringMap.t;
  mutable sccs : string list list;
}

let find_sccs (graph : t) : string list list =
  let ts =
    {
      index = 0;
      stack = [];
      on_stack = StringMap.empty;
      indices = StringMap.empty;
      lowlinks = StringMap.empty;
      sccs = [];
    }
  in

  let rec strongconnect v =
    ts.indices <- StringMap.add v ts.index ts.indices;
    ts.lowlinks <- StringMap.add v ts.index ts.lowlinks;
    ts.index <- ts.index + 1;
    ts.stack <- v :: ts.stack;
    ts.on_stack <- StringMap.add v true ts.on_stack;

    let deps = dependencies_of graph v in
    StringSet.iter
      (fun w ->
        if not (StringMap.mem w ts.indices) then begin
          (* Successor not yet visited *)
          strongconnect w;
          let lowlink_v = StringMap.find v ts.lowlinks in
          let lowlink_w = StringMap.find w ts.lowlinks in
          ts.lowlinks <- StringMap.add v (min lowlink_v lowlink_w) ts.lowlinks
        end
        else if StringMap.find_opt w ts.on_stack |> Option.value ~default:false then begin
          (* Successor is on the stack — back edge *)
          let lowlink_v = StringMap.find v ts.lowlinks in
          let index_w = StringMap.find w ts.indices in
          ts.lowlinks <- StringMap.add v (min lowlink_v index_w) ts.lowlinks
        end)
      deps;

    (* If v is a root of an SCC, pop the stack to collect the component *)
    let index_v = StringMap.find v ts.indices in
    let lowlink_v = StringMap.find v ts.lowlinks in
    if lowlink_v = index_v then begin
      let rec pop_scc acc =
        match ts.stack with
        | [] -> acc
        | w :: rest ->
            ts.stack <- rest;
            ts.on_stack <- StringMap.add w false ts.on_stack;
            let acc = w :: acc in
            if w = v then
              acc
            else
              pop_scc acc
      in
      let scc = pop_scc [] in
      ts.sccs <- scc :: ts.sccs
    end
  in

  StringMap.iter
    (fun v _ ->
      if not (StringMap.mem v ts.indices) then
        strongconnect v)
    graph;

  (* Tarjan produces reverse topo order — reverse to get dependency order *)
  List.rev ts.sccs
;;

(* ------------------------------------------------------------------ *)
(* Public: topologically sorted groups                                  *)
(* ------------------------------------------------------------------ *)

let topo_sorted_groups (graph : t) : group list =
  find_sccs graph
  |> List.map (function
    | [ name ] ->
        (* Check for self-recursion *)
        let deps = dependencies_of graph name in
        if StringSet.mem name deps then
          Recursive [ name ]
        else
          Single name
    | names -> Recursive names)
;;
