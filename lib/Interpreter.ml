module List = struct
  include List
  let is_empty = function
  | [] -> true
  | _ -> false
end

type scope = {
  identifiers: (string, Ast.literal) Hashtbl.t
}

type state = {
  output: string;
  models: string -> Ast.literal option;
  scope: scope list;
}

let add_scope state = begin
  let identifiers = Hashtbl.create 10 in
  let scope = { identifiers } in
  {
    state
    with scope = scope::state.scope
  }
end

let literal_to_string = function
  | Ast.NullLiteral     -> ""
  | Ast.StringLiteral s -> s
  | Ast.IntLiteral i    -> string_of_int i
  | Ast.FloatLiteral f  -> string_of_float f
  | Ast.BoolLiteral b   -> if b then "true" else "false"

let is_true = function
  | Ast.NullLiteral     -> false
  | Ast.BoolLiteral b   -> b
  | Ast.StringLiteral s -> s |> String.trim |> String.length > 0
  | Ast.IntLiteral _    -> true
  | Ast.FloatLiteral _  -> true

let output x state = {
  state with
  output = state.output ^ (literal_to_string x);
}

let rec literal_of_expr state expr = match expr with
  | Ast.LiteralExpression l -> l
  
  | Ast.BlockExpression statements -> eval_block state statements

  | Ast.IdentifierExpression (Id id) -> begin
    let value = state.scope |> List.find_map (fun scope -> Hashtbl.find_opt scope.identifiers id) in
    match value with
    | None -> failwith "Unbound identifier" (* Unbound identifier *)
    | Some v -> v
  end

  | Ast.ArrayExpression expressions ->
    StringLiteral ( 
      expressions
      |> List.map (literal_of_expr state)
      |> List.map literal_to_string
      |> String.concat ""
    )

  | Ast.SymbolExpression _          -> NullLiteral (* TODO: *)

  | Ast.ForInExpression _           -> NullLiteral (* TODO: *)

  | Ast.TemplateExpression template_nodes ->
    StringLiteral ( 
      template_nodes 
      |> List.map (template_to_string state)
      |> String.concat ""
    )
  
  | Ast.ConditionalExpression {condition; consequent; alternate} ->
    if condition |> literal_of_expr state |> is_true
      then consequent |> literal_of_expr state
      else begin match alternate with
        | Some alt -> alt |> literal_of_expr state
        | None -> NullLiteral
      end

  | Ast.UnaryExpression _           -> NullLiteral (* TODO: *)

  | Ast.BinaryExpression _          -> NullLiteral (* TODO: *)

  | Ast.LogicalExpression _         -> NullLiteral (* TODO: *)

and html_attr_to_string state (attr: Ast.attribute) =
  let buf = Buffer.create 64 in
  Buffer.add_string buf attr.key;
  Buffer.add_string buf "=\"";
  literal_of_expr state attr.value |> literal_to_string |> Buffer.add_string buf;
  Buffer.add_string buf "\"";
  Buffer.contents buf

and template_to_string state template = match template with
  | Ast.TextTemplateNode text -> text
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    let buf = Buffer.create 256 in
    Buffer.add_string buf ("<" ^ tag);
    if not (List.is_empty attributes) then begin
      Buffer.add_string buf " ";
      Buffer.add_string buf (List.fold_left (fun acc curr -> acc ^ html_attr_to_string state curr) "" attributes);
    end;
    Buffer.add_string buf ">";
    Buffer.add_string buf (List.fold_left (fun acc curr -> acc ^ template_to_string state curr) "" children);
    Buffer.add_string buf ("</" ^ tag ^ ">");
    Buffer.contents buf
  | Ast.ExpressionTemplateNode expr -> literal_to_string (literal_of_expr state expr)
  | Ast.ComponentTemplateNode { identifier=_; attributes=_; children=_; _ } -> "" (* TODO: *)

and eval_block state = begin
  let state = state |> add_scope in
  let result = List.fold_left (fun _acc curr -> eval_statement state curr) Ast.NullLiteral in
  result
end

and eval_statement state stmt = begin 
  match stmt with
  | Ast.BreakStmt           -> NullLiteral (* TODO: *)
  | Ast.ContinueStmt        -> NullLiteral (* TODO: *)
  | Ast.DeclarationStmt { nullable; left = Id ident; right; } -> begin
    match state.scope with
    | [] -> assert false
    | scope::_ -> 
      let literal = literal_of_expr state right in
      begin match literal with
      | NullLiteral when not nullable -> failwith "Not Nullable!!"
      | _ -> Hashtbl.replace scope.identifiers ident literal
      end;
    NullLiteral
  end
  | Ast.ExpressionStmt expr -> literal_of_expr state expr
end
let eval_declaration state declaration = match declaration with
  | Ast.ComponentDeclaration { body; _ } -> eval_block state body
  | Ast.SiteDeclaration { body; _ }      -> eval_block state body
  | Ast.PageDeclaration { body; _ }      -> eval_block state body
  | Ast.StoreDeclaration { body; _ }     -> eval_block state body

let eval ~state ast =
  List.fold_left (fun state curr -> output (eval_declaration state curr) state) state ast

let init_state ?(models=(fun _ -> None)) () = begin
  let state = {
    output = "";
    scope = [];
    models;
  } in
  state
end

let file_contents chan = really_input_string chan (in_channel_length chan)

let from_file ?(models=[]) ~filename () =
  let state  = init_state ~models:(fun key -> List.assoc key models) () in
  let src    = open_in filename |> file_contents in
  let parser = Parser.make ~filename src in
  let ast    = Parser.scan parser in
  let resulting_state = eval ~state ast in
  resulting_state.output
