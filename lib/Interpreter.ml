type scope = {
  identifiers: (string, Ast.expression) Hashtbl.t
}

type context = {
  output: string;
  models: string -> Ast.literal option;
  mutable scope: scope list;
}

let add_scope ctx = begin
  let identifiers = Hashtbl.create 10 in
  let scope = { identifiers } in
  ctx.scope <- scope::ctx.scope;
  ctx
end

let pop_scope ctx = begin
  ctx.scope <- (match ctx.scope with
  | [] -> []
  | _hd::tl -> tl);
  ctx
end

let literal_to_string = function
  | Ast.StringLiteral s -> s
  | Ast.IntLiteral i    -> string_of_int i
  | Ast.FloatLiteral f  -> string_of_float f
  | Ast.BoolLiteral b   -> if b then "true" else "false"

let output x ctx = {
  ctx with
  output = ctx.output ^ (literal_to_string x);
}

let cross_fold f list ctx = List.fold_left (fun acc curr -> f curr acc) ctx list

let rec eval_html_attr (attr: Ast.attribute) ctx =
  ctx
  |> output (StringLiteral attr.key) 
  |> output (StringLiteral "=\"")
  |> eval_expr attr.value
  |> output (StringLiteral "\"")

and eval_template template ctx = match template with
  | Ast.TextTemplateNode text -> ctx |> output (StringLiteral text)
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    ctx
    |> output (StringLiteral (Printf.sprintf "<%s " tag))
    |> cross_fold eval_html_attr attributes
    |> output (StringLiteral ">")
    |> cross_fold eval_template children
    |> output (StringLiteral ("</" ^ tag ^ ">"))
  | Ast.ExpressionTemplateNode expr -> ctx |> eval_expr expr
  | Ast.ComponentTemplateNode { identifier=_; attributes=_; children=_; _ } -> ctx (* TODO: *)

and eval_expr expr ctx = match expr with
  | Ast.LiteralExpression l         -> ctx |> output l
  | Ast.BlockExpression statement   ->
    ctx
      |> add_scope
      |> cross_fold eval_statement statement
      |> pop_scope
  | Ast.IdentifierExpression (Id id) -> begin
    let value = ctx.scope |> List.find_map (fun scope -> Hashtbl.find_opt scope.identifiers id) in
    match value with
    | None -> failwith "Unbound identifier" (* Unbound identifier *)
    | Some v -> ctx |> eval_expr v
  end
  | Ast.ArrayExpression _           -> ctx (* TODO: *)
  | Ast.SymbolExpression _          -> ctx (* TODO: *)
  | Ast.ForInExpression _           -> ctx (* TODO: *)
  | Ast.TemplateExpression t        -> ctx |> cross_fold eval_template t
  | Ast.ConditionalExpression _     -> ctx (* TODO: *)
  | Ast.UnaryExpression _           -> ctx (* TODO: *)
  | Ast.BinaryExpression _          -> ctx (* TODO: *)
  | Ast.LogicalExpression _         -> ctx (* TODO: *)

and eval_statement stmt ctx = match stmt with
  | Ast.BreakStmt           -> ctx (* TODO: *)
  | Ast.ContinueStmt        -> ctx (* TODO: *)
  | Ast.DeclarationStmt { nullable=_; left = Id ident; right; } -> begin
    match ctx.scope with
    | [] -> assert false
    | scope::_ -> Hashtbl.replace scope.identifiers ident right;
    ctx
  end
  | Ast.ExpressionStmt expr -> ctx |> eval_expr expr

let eval_declaration declaration ctx = match declaration with
  | Ast.ComponentDeclaration { body; _ } -> ctx |> cross_fold eval_statement body
  | Ast.SiteDeclaration { body; _ }      -> ctx |> cross_fold eval_statement body
  | Ast.PageDeclaration { body; _ }      -> ctx |> cross_fold eval_statement body
  | Ast.StoreDeclaration { body; _ }     -> ctx |> cross_fold eval_statement body

let eval ~ctx ast = ctx |> cross_fold eval_declaration ast

let init_context ?(models=(fun _ -> None)) () = begin
  let ctx = {
    output = "";
    scope = [{ identifiers = Hashtbl.create 10 }];
    models;
  } in
  ctx
end

let file_contents chan = really_input_string chan (in_channel_length chan)


let from_file ?(models=[]) ~filename () =
  let ctx    = init_context ~models:(fun key -> List.assoc key models) () in
  let src    = open_in filename |> file_contents in
  let parser = Parser.make ~filename src in
  let ast    = Parser.scan parser in
  let resulting_ctx = eval ~ctx ast in
  resulting_ctx.output
