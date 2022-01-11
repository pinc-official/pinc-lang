type scope = {
  identifiers: (string, Ast.expression) Hashtbl.t
}

type context = {
  output: Buffer.t;
  models: string -> Ast.literal option;
  mutable scope: scope list;
}

let add_scope ctx = begin
  let identifiers = Hashtbl.create 10 in
  let scope = { identifiers } in
  ctx.scope <- scope::ctx.scope
end

let pop_scope ctx = begin
  ctx.scope <- match ctx.scope with
  | [] -> []
  | _hd::tl -> tl
end

let literal_to_string = function
  | Ast.StringLiteral s -> s
  | Ast.IntLiteral i    -> string_of_int i
  | Ast.FloatLiteral f  -> string_of_float f
  | Ast.BoolLiteral b   -> if b then "true" else "false"

let output x ctx = Buffer.add_string ctx.output (literal_to_string x); ctx

let rec eval_template ctx = function
  | Ast.TextTemplateNode text -> ctx |> output (StringLiteral text)
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    let ctx = ctx |> output (StringLiteral (Printf.sprintf "<%s " tag)) in
    let ctx = attributes |> List.fold_left (fun ctx (attr: Ast.attribute) -> (
      let ctx = ctx |> output (StringLiteral attr.key) in
      let ctx = ctx |> output (StringLiteral "=\"") in
      let ctx = eval_expr ctx attr.value in
      let ctx = ctx |> output (StringLiteral "\"") in
      ctx
    )) ctx in
    let ctx = ctx |> output (StringLiteral ">") in
    let ctx = children |> List.fold_left eval_template ctx in
    let ctx = ctx |> output (StringLiteral ("</" ^ tag ^ ">")) in
    ctx
  | Ast.ExpressionTemplateNode expr -> eval_expr ctx expr
  | Ast.ComponentTemplateNode { identifier=_; attributes=_; children=_; _ } -> ctx (* TODO: *)

and eval_expr ctx = function
  | Ast.LiteralExpression l         -> ctx |> output l
  | Ast.BlockExpression s           -> 
    add_scope ctx;
    let ctx = s |> List.fold_left eval_statement ctx in
    pop_scope ctx;
    ctx
  | Ast.IdentifierExpression (Id id) -> begin
    let value = ctx.scope |> List.find_map (fun scope -> (
      Hashtbl.find_opt scope.identifiers id
    )) in
    match value with
    | None -> failwith "Unbound identifier" (* Unbound identifier *)
    | Some v -> eval_expr ctx v
  end
  | Ast.ArrayExpression _           -> ctx (* TODO: *)
  | Ast.SymbolExpression _          -> ctx (* TODO: *)
  | Ast.ForInExpression _           -> ctx (* TODO: *)
  | Ast.TemplateExpression t        -> t |> List.fold_left eval_template ctx
  | Ast.ConditionalExpression _     -> ctx (* TODO: *)
  | Ast.UnaryExpression _           -> ctx (* TODO: *)
  | Ast.BinaryExpression _          -> ctx (* TODO: *)
  | Ast.LogicalExpression _         -> ctx (* TODO: *)

and eval_statement ctx = function
  | Ast.BreakStmt           -> ctx (* TODO: *)
  | Ast.ContinueStmt        -> ctx (* TODO: *)
  | Ast.DeclarationStmt { nullable=_; left = Id ident; right; } -> begin
    match ctx.scope with
    | [] -> assert false
    | scope::_ -> Hashtbl.replace scope.identifiers ident right;
    ctx
  end
  | Ast.ExpressionStmt expr -> eval_expr ctx expr

let eval_declaration ctx = function
  | Ast.ComponentDeclaration { body; _ } -> body |> List.fold_left eval_statement ctx
  | Ast.SiteDeclaration { body; _ }      -> body |> List.fold_left eval_statement ctx
  | Ast.PageDeclaration { body; _ }      -> body |> List.fold_left eval_statement ctx
  | Ast.StoreDeclaration { body; _ }     -> body |> List.fold_left eval_statement ctx

let eval ~ctx ast = ast |> List.fold_left eval_declaration ctx

let init_context ?(models=(fun _ -> None)) () =
  let output = Buffer.create 1024 in
  let top_level_identifiers = Hashtbl.create 10 in
  let ctx = {
    output;
    scope = [{ identifiers = top_level_identifiers }];
    models;
  } in
  ctx

let file_contents chan = really_input_string chan (in_channel_length chan)


let from_file ?(models=[]) ~filename () =
  let ctx    = init_context ~models:(fun key -> List.assoc key models) () in
  let src    = open_in filename |> file_contents in
  let parser = Parser.make ~filename src in
  let ast    = Parser.scan parser in
  let resulting_ctx = eval ~ctx ast in
  Buffer.contents resulting_ctx.output