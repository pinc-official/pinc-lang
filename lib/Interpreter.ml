type context = {
  models: string -> Ast.literal option;
  output: string -> unit
}

let rec eval_literal ctx = function
  | Ast.StringLiteral s -> ctx.output s
  | Ast.IntLiteral i    -> ctx.output (string_of_int i)
  | Ast.FloatLiteral f  -> ctx.output (string_of_float f)
  | Ast.BoolLiteral b   -> ctx.output (if b then "true" else "false")

and eval_template ctx = function
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    ctx.output "<";
    ctx.output tag;
    ctx.output " ";
    attributes |> List.iter (fun (attr: Ast.attribute) -> (
      ctx.output attr.key;
      ctx.output "=\"";
      eval_expr ctx attr.value;
      ctx.output "\"";
    ));
    ctx.output ">";
    children |> List.iter (eval_template ctx);
    ctx.output ("</" ^ tag ^ ">");
  | Ast.ExpressionTemplateNode expr -> eval_expr ctx expr
  | Ast.TextTemplateNode text       -> ctx.output text
  | Ast.ComponentTemplateNode { identifier=_; attributes=_; children=_; _ } -> () (* TODO: *)

and eval_expr ctx = function
  | Ast.BlockExpression s           -> s |> List.iter (eval_statement ctx)
  | Ast.IdentifierExpression (Id _) -> () (* TODO: *)
  | Ast.LiteralExpression l         -> l |> eval_literal ctx
  | Ast.ArrayExpression _           -> () (* TODO: *)
  | Ast.SymbolExpression _          -> () (* TODO: *)
  | Ast.ForInExpression _           -> () (* TODO: *)
  | Ast.TemplateExpression t        -> t |> List.iter (eval_template ctx)
  | Ast.ConditionalExpression _     -> () (* TODO: *)
  | Ast.UnaryExpression _           -> () (* TODO: *)
  | Ast.BinaryExpression _          -> () (* TODO: *)
  | Ast.LogicalExpression _         -> () (* TODO: *)

and eval_statement ctx = function
  | Ast.BreakStmt           -> () (* TODO: *)
  | Ast.ContinueStmt        -> () (* TODO: *)
  | Ast.DeclarationStmt _   -> () (* TODO: *)
  | Ast.ExpressionStmt expr -> eval_expr ctx expr

let eval_declaration ctx = function
  | Ast.ComponentDeclaration { body; _ } -> body |> List.iter (eval_statement ctx)
  | Ast.SiteDeclaration { body; _ }      -> body |> List.iter (eval_statement ctx)
  | Ast.PageDeclaration { body; _ }      -> body |> List.iter (eval_statement ctx)
  | Ast.StoreDeclaration { body; _ }     -> body |> List.iter (eval_statement ctx)

let eval ~ctx ast = List.iter (eval_declaration ctx) ast

let init_context ?(models=(fun _ -> None)) output =
  let ctx = {
    models;
    output
  } in
  ctx

let file_contents chan = really_input_string chan (in_channel_length chan)

let from_file ?(models=[]) ~output filename =
  let ctx = init_context ~models:(fun key -> List.assoc key models) output in
  let src = open_in filename |> file_contents in
  let parser = Parser.make ~filename src in
  let ast = Parser.scan parser in
  eval ~ctx ast