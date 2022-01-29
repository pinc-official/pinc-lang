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

module Literal = struct
  type t = Ast.literal

  let rec to_string = function
    | Ast.NullLiteral     -> ""
    | Ast.StringLiteral s -> s
    | Ast.IntLiteral i    -> string_of_int i
    | Ast.FloatLiteral f  -> string_of_float f
    | Ast.BoolLiteral b   -> if b then "true" else "false"
    | Ast.ArrayLiteral l  -> l |> List.map to_string |> String.concat ""

  let is_true = function
    | Ast.NullLiteral     -> false
    | Ast.BoolLiteral b   -> b
    | Ast.StringLiteral s -> s |> String.trim |> String.length > 0
    | Ast.IntLiteral _    -> true
    | Ast.FloatLiteral _  -> true
    | Ast.ArrayLiteral l  -> l |> List.length > 0 (* TODO: Is a list of only null values also not true? *)

  let neg l = not (is_true l)

  let is_numeric t = match t with
    | Ast.IntLiteral _   -> true
    | Ast.FloatLiteral _ -> true
    | _ -> false

  let int_of_literal t = match t with
    | Ast.IntLiteral i   -> Some i
    | Ast.FloatLiteral f -> Some (int_of_float f)
    | _ -> None
  
  let equal a b = match a, b with
    | Ast.StringLiteral a, Ast.StringLiteral b -> String.equal a b
    | Ast.IntLiteral a, Ast.IntLiteral b       -> a = b
    | Ast.FloatLiteral a, Ast.FloatLiteral b   -> a = b
    | Ast.BoolLiteral a, Ast.BoolLiteral b     -> a = b
    | Ast.ArrayLiteral a, Ast.ArrayLiteral b   -> a = b
    | Ast.NullLiteral, Ast.NullLiteral         -> true
    | _ -> false

  let compare a b = match a, b with
    | Ast.StringLiteral a, Ast.StringLiteral b -> String.compare a b
    | Ast.IntLiteral a, Ast.IntLiteral b       -> Int.compare a b
    | Ast.FloatLiteral a, Ast.FloatLiteral b   -> Float.compare a b
    | Ast.BoolLiteral a, Ast.BoolLiteral b     -> Bool.compare a b
    | Ast.ArrayLiteral a, Ast.ArrayLiteral b   -> List.length a - List.length b
    | Ast.NullLiteral, Ast.NullLiteral         -> 0
    | _ -> failwith "Nope"

end

let output x state = {
  state with
  output = state.output ^ (Literal.to_string x);
}

let add_literal_to_scope ~ident ~literal state = match state.scope with
| [] -> assert false
| scope::_ -> Hashtbl.replace scope.identifiers ident literal

let rec literal_of_expr state expr = match expr with
  | Ast.LiteralExpression l -> l
  
  | Ast.BlockExpression statements -> eval_block state statements

  | Ast.IdentifierExpression (Id id) -> begin
    let value = state.scope |> List.find_map (fun scope -> Hashtbl.find_opt scope.identifiers id) in
    match value with
    | None -> failwith "Unbound identifier" (* Unbound identifier *)
    | Some v -> v
  end

  | Ast.ArrayExpression expressions -> ArrayLiteral (expressions |> List.map (literal_of_expr state))

  | Ast.TagExpression _          -> NullLiteral (* TODO: *)

  | Ast.ForInExpression { iterator=Id ident; iterable; reverse; body; } -> begin
    let iterable = iterable |> literal_of_expr state in
    let state = state |> add_scope in
    match iterable with
    | Ast.ArrayLiteral l ->
      let loop literal = begin
        state |> add_literal_to_scope ~ident ~literal;
        eval_block state body
      end in
      let map = if reverse then List.rev_map else List.map in
      Ast.ArrayLiteral (l |> map loop)

    | Ast.StringLiteral s ->
      let explode s = List.init (String.length s) (String.unsafe_get s) in
      let loop c = begin
        let literal = Ast.StringLiteral (String.make 1 c) in
        state |> add_literal_to_scope ~ident ~literal;
        eval_block state body
      end in
      let map = if reverse then List.rev_map else List.map in
      Ast.ArrayLiteral (s |> explode |> map loop)

    | NullLiteral -> ArrayLiteral []
    | IntLiteral _ -> failwith "Cannot iterate over int value"
    | FloatLiteral _ -> failwith "Cannot iterate over float value"
    | BoolLiteral _ -> failwith "Cannot iterate over boolean value"
  end

  | Ast.ForInRangeExpression { iterator=Id ident; reverse; from; upto; body } -> begin
    let from = from |> literal_of_expr state in
    let upto = upto |> literal_of_expr state in

    let (from, upto) = begin match from, upto with
    | Ast.IntLiteral from, Ast.IntLiteral upto -> from, upto
    | Ast.IntLiteral _, _                      -> failwith "Can't construct range in for loop. The end of your range is not of type int."
    | _               , Ast.IntLiteral _       -> failwith "Can't construct range in for loop. The start of your range is not of type int."
    | _               , _                      -> failwith "Can't construct range in for loop. The start and end of your range are not of type int."
    end in

    let state = state |> add_scope in

    let rec loop ~dir acc from upto =
      let (><) = match dir with | `Up -> (>) | `Down -> (<) in
      let (-+) = match dir with | `Up -> (-) | `Down -> (+) in
      if upto >< from then
        let upto = (upto -+ 1) in
        let literal = Ast.IntLiteral upto in
        state |> add_literal_to_scope ~ident ~literal;
        let r = eval_block state body in
        loop (r :: acc) from upto ~dir
      else
        acc
    in

    let result = if reverse then
      loop [] from upto ~dir:`Down
    else
      loop [] from upto ~dir:`Up
    in

    Ast.ArrayLiteral result
  end

  | Ast.TemplateExpression template_nodes ->
    StringLiteral ( 
      template_nodes 
      |> List.map (template_to_string state)
      |> String.concat ""
    )
  
  | Ast.ConditionalExpression {condition; consequent; alternate} ->
    if condition |> literal_of_expr state |> Literal.is_true
      then consequent |> literal_of_expr state
      else begin match alternate with
        | Some alt -> alt |> literal_of_expr state
        | None -> NullLiteral
      end

  | Ast.UnaryExpression { operator; argument; } ->
    let res = argument |> literal_of_expr state in
    begin match operator, res with
    | Ast.Operator.NOT, literal -> Ast.BoolLiteral (Literal.neg literal)
    | Ast.Operator.NEGATIVE, IntLiteral i -> Ast.IntLiteral (Int.neg i)
    | Ast.Operator.NEGATIVE, FloatLiteral f -> Ast.FloatLiteral (Float.neg f)
    | Ast.Operator.NEGATIVE, _ -> failwith "Invalid usage of unary - operator"
    end

  | Ast.BinaryExpression { left; operator; right; } ->
    let a = lazy (left |> literal_of_expr state) in
    let b = lazy (right |> literal_of_expr state) in
    begin match operator with
    | Ast.Operator.NOT_EQUAL -> BoolLiteral (not (Literal.equal (Lazy.force a) (Lazy.force b)))
    | Ast.Operator.EQUAL -> BoolLiteral (Literal.equal (Lazy.force a) (Lazy.force b))
    | Ast.Operator.LESS -> BoolLiteral (Literal.compare (Lazy.force a) (Lazy.force b) < 0)
    | Ast.Operator.LESS_EQUAL -> BoolLiteral (Literal.compare (Lazy.force a) (Lazy.force b) <= 0)
    | Ast.Operator.GREATER -> BoolLiteral (Literal.compare (Lazy.force a) (Lazy.force b) > 0)
    | Ast.Operator.GREATER_EQUAL -> BoolLiteral (Literal.compare (Lazy.force a) (Lazy.force b) >= 0)
    | Ast.Operator.CONCAT -> (
      match Lazy.force a, Lazy.force b with
      | Ast.StringLiteral a, Ast.StringLiteral b -> Ast.StringLiteral (a ^ b)
      | _ -> failwith "Trying to concat non string literals."
    )
    | Ast.Operator.PLUS -> (
      match Lazy.force a, Lazy.force b with
      | Ast.IntLiteral a, Ast.IntLiteral b     -> Ast.IntLiteral (a + b)
      | Ast.FloatLiteral a, Ast.FloatLiteral b -> Ast.FloatLiteral (a +. b)
      | Ast.FloatLiteral a, Ast.IntLiteral b   -> Ast.FloatLiteral (a +. float_of_int b)
      | Ast.IntLiteral a, Ast.FloatLiteral b   -> Ast.FloatLiteral (float_of_int a +. b)
      | _ -> failwith "Trying to add non numeric literals."
    )
    | Ast.Operator.MINUS -> (
      match Lazy.force a, Lazy.force b with
      | Ast.IntLiteral a, Ast.IntLiteral b     -> Ast.IntLiteral (a - b)
      | Ast.FloatLiteral a, Ast.FloatLiteral b -> Ast.FloatLiteral (a -. b)
      | Ast.FloatLiteral a, Ast.IntLiteral b   -> Ast.FloatLiteral (a -. float_of_int b)
      | Ast.IntLiteral a, Ast.FloatLiteral b   -> Ast.FloatLiteral (float_of_int a -. b)
      | _ -> failwith "Trying to subtract non numeric literals."
    )
    | Ast.Operator.TIMES -> (
      match Lazy.force a, Lazy.force b with
      | Ast.IntLiteral a, Ast.IntLiteral b     -> Ast.IntLiteral (a * b)
      | Ast.FloatLiteral a, Ast.FloatLiteral b -> Ast.FloatLiteral (a *. b)
      | Ast.FloatLiteral a, Ast.IntLiteral b   -> Ast.FloatLiteral (a *. float_of_int b)
      | Ast.IntLiteral a, Ast.FloatLiteral b   -> Ast.FloatLiteral (float_of_int a *. b)
      | _ -> failwith "Trying to multiply non numeric literals."
    )
    | Ast.Operator.DIV -> (
      match Lazy.force a, Lazy.force b with
      | Ast.IntLiteral a, Ast.IntLiteral b     -> Ast.IntLiteral (a / b)
      | Ast.FloatLiteral a, Ast.FloatLiteral b -> Ast.FloatLiteral (a /. b)
      | Ast.FloatLiteral a, Ast.IntLiteral b   -> Ast.FloatLiteral (a /. float_of_int b)
      | Ast.IntLiteral a, Ast.FloatLiteral b   -> Ast.FloatLiteral (float_of_int a /. b)
      | _ -> failwith "Trying to divide non numeric literals."
    )
    | Ast.Operator.POW -> (
      match Lazy.force a, Lazy.force b with
      | Ast.IntLiteral a, Ast.IntLiteral b     -> 
        let r = (float_of_int a ** float_of_int b) in
        if Float.is_integer r 
          then Ast.IntLiteral (int_of_float r) 
          else Ast.FloatLiteral r
      | Ast.FloatLiteral a, Ast.FloatLiteral b ->
        let r =  (a ** b) in
        if Float.is_integer r 
          then Ast.IntLiteral (int_of_float r) 
          else Ast.FloatLiteral r
      | Ast.FloatLiteral a, Ast.IntLiteral b   -> 
        let r = (a ** float_of_int b) in
        if Float.is_integer r 
          then Ast.IntLiteral (int_of_float r) 
          else Ast.FloatLiteral r
      | Ast.IntLiteral a, Ast.FloatLiteral b   ->
        let r = (float_of_int a ** b) in
        if Float.is_integer r 
          then Ast.IntLiteral (int_of_float r) 
          else Ast.FloatLiteral r
      | _ -> failwith "Trying to raise non numeric literals."
    )
    | Ast.Operator.AND -> BoolLiteral (Literal.is_true (Lazy.force a) && Literal.is_true (Lazy.force b))
    | Ast.Operator.OR -> BoolLiteral (Literal.is_true (Lazy.force a) || Literal.is_true (Lazy.force b))
    end

and html_attr_to_string state (attr: Ast.attribute) =
  let value = literal_of_expr state attr.value |> Literal.to_string in
  let key = attr.key in
  Printf.sprintf "%s=\"%s\"" key value

and template_to_string state template = match template with
  | Ast.TextTemplateNode text -> text
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    let attributes = 
      attributes 
      |> List.map (html_attr_to_string state) 
      |> String.concat " "
    in
    let children = 
      children 
      |> List.map (template_to_string state)
      |> String.concat ""
    in
    Printf.sprintf "<%s%s>%s</%s>"
      tag
      (if attributes <> "" then " " ^ attributes else "")
      children
      tag
  | Ast.ExpressionTemplateNode expr -> Literal.to_string (literal_of_expr state expr)
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
    let literal = literal_of_expr state right in
    begin match literal with
    | NullLiteral when not nullable -> failwith "Not Nullable!!"
    | _ -> state |> add_literal_to_scope ~ident ~literal
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
