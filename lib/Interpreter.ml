type scope = { identifiers: (string, Ast.Literal.t) Hashtbl.t }

type state = {
  output: string;
  models: string -> Ast.Literal.t option;
  scope: scope list;
}

let add_scope state =
  let identifiers = Hashtbl.create 10 in
  let scope = { identifiers } in
  { state with scope = scope :: state.scope }
;;

let output x state =
  { state with output = state.output ^ Ast.Literal.to_string x }
;;

let add_literal_to_scope ~ident ~literal state =
  match state.scope with
  | [] -> assert false
  | scope :: _ -> Hashtbl.replace scope.identifiers ident literal
;;

let rec literal_of_expr state expr =
  match expr with
  | Ast.LiteralExpression l -> l
  | Ast.BlockExpression statements -> eval_block state statements
  | Ast.IdentifierExpression (Id id) ->
    let value =
      state.scope
      |> List.find_map (fun scope -> Hashtbl.find_opt scope.identifiers id)
    in
    ( match value with
    | None -> failwith "Unbound identifier" (* Unbound identifier *)
    | Some v -> v
    )
  | Ast.ArrayExpression expressions ->
    Ast.Literal.Array (expressions |> List.map (literal_of_expr state))
  | Ast.TagExpression _ -> Ast.Literal.Null (* TODO: *)
  | Ast.ForInExpression { iterator = Id ident; iterable; reverse; body } ->
    let iterable = iterable |> literal_of_expr state in
    let state = state |> add_scope in
    ( match iterable with
    | Ast.Literal.Array l ->
      let loop literal =
        state |> add_literal_to_scope ~ident ~literal;
        eval_block state body
      in
      let map = if reverse then List.rev_map else List.map in
      Ast.Literal.Array (l |> map loop)
    | Ast.Literal.String s ->
      let explode s = List.init (String.length s) (String.unsafe_get s) in
      let loop c =
        let literal = Ast.Literal.String (String.make 1 c) in
        state |> add_literal_to_scope ~ident ~literal;
        eval_block state body
      in
      let map = if reverse then List.rev_map else List.map in
      Ast.Literal.Array (s |> explode |> map loop)
    | Ast.Literal.Null -> Ast.Literal.Array []
    | Ast.Literal.Int _ -> failwith "Cannot iterate over int value"
    | Ast.Literal.Float _ -> failwith "Cannot iterate over float value"
    | Ast.Literal.Bool _ -> failwith "Cannot iterate over boolean value"
    )
  | Ast.ForInRangeExpression
      { iterator = Id ident; reverse; from; upto; inclusive; body } ->
    let from = from |> literal_of_expr state in
    let upto = upto |> literal_of_expr state in
    let from, upto =
      match from, upto with
      | Ast.Literal.Int from, Ast.Literal.Int upto -> from, upto
      | Ast.Literal.Int _, _ ->
        failwith
          "Can't construct range in for loop. The end of your range is not of \
           type int."
      | _, Ast.Literal.Int _ ->
        failwith
          "Can't construct range in for loop. The start of your range is not \
           of type int."
      | _, _ ->
        failwith
          "Can't construct range in for loop. The start and end of your range \
           are not of type int."
    in
    let state = state |> add_scope in
    let rec loop acc reverse from upto =
      if (reverse && upto < from) || ((not reverse) && upto > from)
      then (
        let upto = if reverse then upto + 1 else upto - 1 in
        let literal = Ast.Literal.Int upto in
        state |> add_literal_to_scope ~ident ~literal;
        let r = eval_block state body in
        loop (r :: acc) reverse from upto
      )
      else acc
    in
    let upto =
      if inclusive then if reverse then upto - 1 else upto + 1 else upto
    in
    let result = loop [] reverse from upto in
    Ast.Literal.Array result
  | Ast.TemplateExpression template_nodes ->
    Ast.Literal.String
      (template_nodes |> List.map (template_to_string state) |> String.concat "")
  | Ast.ConditionalExpression { condition; consequent; alternate } ->
    if condition |> literal_of_expr state |> Ast.Literal.is_true
    then consequent |> literal_of_expr state
    else (
      match alternate with
      | Some alt -> alt |> literal_of_expr state
      | None -> Ast.Literal.Null
    )
  | Ast.UnaryExpression { operator; argument } ->
    let res = argument |> literal_of_expr state in
    ( match operator, res with
    | Ast.Operators.Unary.NOT, literal ->
      Ast.Literal.Bool (Ast.Literal.negate literal)
    | Ast.Operators.Unary.NEGATIVE, Ast.Literal.Int i ->
      Ast.Literal.Int (Int.neg i)
    | Ast.Operators.Unary.NEGATIVE, Ast.Literal.Float f ->
      Ast.Literal.Float (Float.neg f)
    | Ast.Operators.Unary.NEGATIVE, _ ->
      failwith "Invalid usage of unary - operator"
    )
  | Ast.BinaryExpression { left; operator; right } ->
    let a = lazy (left |> literal_of_expr state) in
    let b = lazy (right |> literal_of_expr state) in
    ( match operator with
    | Ast.Operators.Binary.NOT_EQUAL ->
      Ast.Literal.Bool (not (Ast.Literal.equal (Lazy.force a, Lazy.force b)))
    | Ast.Operators.Binary.EQUAL ->
      Ast.Literal.Bool (Ast.Literal.equal (Lazy.force a, Lazy.force b))
    | Ast.Operators.Binary.LESS ->
      Ast.Literal.Bool (Ast.Literal.compare (Lazy.force a, Lazy.force b) < 0)
    | Ast.Operators.Binary.LESS_EQUAL ->
      Ast.Literal.Bool (Ast.Literal.compare (Lazy.force a, Lazy.force b) <= 0)
    | Ast.Operators.Binary.GREATER ->
      Ast.Literal.Bool (Ast.Literal.compare (Lazy.force a, Lazy.force b) > 0)
    | Ast.Operators.Binary.GREATER_EQUAL ->
      Ast.Literal.Bool (Ast.Literal.compare (Lazy.force a, Lazy.force b) >= 0)
    | Ast.Operators.Binary.CONCAT ->
      ( match Lazy.force a, Lazy.force b with
      | Ast.Literal.String a, Ast.Literal.String b -> Ast.Literal.String (a ^ b)
      | _ -> failwith "Trying to concat non string literals."
      )
    | Ast.Operators.Binary.PLUS ->
      ( match Lazy.force a, Lazy.force b with
      | Ast.Literal.Int a, Ast.Literal.Int b -> Ast.Literal.Int (a + b)
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a +. b)
      | Ast.Literal.Float a, Ast.Literal.Int b ->
        Ast.Literal.Float (a +. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b ->
        Ast.Literal.Float (float_of_int a +. b)
      | _ -> failwith "Trying to add non numeric literals."
      )
    | Ast.Operators.Binary.MINUS ->
      ( match Lazy.force a, Lazy.force b with
      | Ast.Literal.Int a, Ast.Literal.Int b -> Ast.Literal.Int (a - b)
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a -. b)
      | Ast.Literal.Float a, Ast.Literal.Int b ->
        Ast.Literal.Float (a -. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b ->
        Ast.Literal.Float (float_of_int a -. b)
      | _ -> failwith "Trying to subtract non numeric literals."
      )
    | Ast.Operators.Binary.TIMES ->
      ( match Lazy.force a, Lazy.force b with
      | Ast.Literal.Int a, Ast.Literal.Int b -> Ast.Literal.Int (a * b)
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a *. b)
      | Ast.Literal.Float a, Ast.Literal.Int b ->
        Ast.Literal.Float (a *. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b ->
        Ast.Literal.Float (float_of_int a *. b)
      | _ -> failwith "Trying to multiply non numeric literals."
      )
    | Ast.Operators.Binary.DIV ->
      ( match Lazy.force a, Lazy.force b with
      | Ast.Literal.Int a, Ast.Literal.Int b ->
        let result = float_of_int a /. float_of_int b in
        Ast.Literal.Float result
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a /. b)
      | Ast.Literal.Float a, Ast.Literal.Int b ->
        Ast.Literal.Float (a /. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b ->
        Ast.Literal.Float (float_of_int a /. b)
      | _ -> failwith "Trying to divide non numeric literals."
      )
    | Ast.Operators.Binary.POW ->
      ( match Lazy.force a, Lazy.force b with
      | Ast.Literal.Int a, Ast.Literal.Int b ->
        let r = float_of_int a ** float_of_int b in
        Ast.Literal.Float r
      | Ast.Literal.Float a, Ast.Literal.Float b ->
        let r = a ** b in
        Ast.Literal.Float r
      | Ast.Literal.Float a, Ast.Literal.Int b ->
        let r = a ** float_of_int b in
        Ast.Literal.Float r
      | Ast.Literal.Int a, Ast.Literal.Float b ->
        let r = float_of_int a ** b in
        Ast.Literal.Float r
      | _ -> failwith "Trying to raise non numeric literals."
      )
    | Ast.Operators.Binary.AND ->
      Ast.Literal.Bool
        (Ast.Literal.is_true (Lazy.force a)
        && Ast.Literal.is_true (Lazy.force b)
        )
    | Ast.Operators.Binary.OR ->
      Ast.Literal.Bool
        (Ast.Literal.is_true (Lazy.force a)
        || Ast.Literal.is_true (Lazy.force b)
        )
    )

and html_attr_to_string state (attr : Ast.attribute) =
  let value = literal_of_expr state attr.value |> Ast.Literal.to_string in
  let key = attr.key in
  Printf.sprintf "%s=\"%s\"" key value

and template_to_string state template =
  match template with
  | Ast.TextTemplateNode text -> text
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    let attributes =
      attributes |> List.map (html_attr_to_string state) |> String.concat " "
    in
    let children =
      children |> List.map (template_to_string state) |> String.concat ""
    in
    Printf.sprintf
      "<%s%s>%s</%s>"
      tag
      (if attributes <> "" then " " ^ attributes else "")
      children
      tag
  | Ast.ExpressionTemplateNode expr ->
    Ast.Literal.to_string (literal_of_expr state expr)
  | Ast.ComponentTemplateNode
      { identifier = _; attributes = _; children = _; _ } -> (* TODO: *) ""

and eval_block state =
  let state = state |> add_scope in
  let result =
    List.fold_left (fun _acc curr -> eval_statement state curr) Ast.Literal.Null
  in
  result

and eval_statement state stmt =
  match stmt with
  | Ast.BreakStmt -> Ast.Literal.Null (* TODO: *)
  | Ast.ContinueStmt -> Ast.Literal.Null (* TODO: *)
  | Ast.DeclarationStmt { nullable; left = Id ident; right } ->
    let literal = literal_of_expr state right in
    ( match literal with
    | Ast.Literal.Null when not nullable -> failwith "Not Nullable!!"
    | _ -> state |> add_literal_to_scope ~ident ~literal
    );
    Ast.Literal.Null
  | Ast.ExpressionStmt expr -> literal_of_expr state expr
;;

let eval_declaration state declaration =
  match declaration with
  | Ast.ComponentDeclaration { body; _ } -> eval_block state body
  | Ast.SiteDeclaration { body; _ } -> eval_block state body
  | Ast.PageDeclaration { body; _ } -> eval_block state body
  | Ast.StoreDeclaration { body; _ } -> eval_block state body
;;

let eval ~state ast =
  List.fold_left
    (fun state curr -> output (eval_declaration state curr) state)
    state
    ast
;;

let init_state ?(models = fun _ -> None) () =
  let state = { output = ""; scope = []; models } in
  state
;;

let file_contents chan = really_input_string chan (in_channel_length chan)

let from_file ?(models = []) ~filename () =
  let state = init_state ~models:(fun key -> List.assoc key models) () in
  let src = open_in filename |> file_contents in
  let parser = Parser.make ~filename src in
  let ast = Parser.scan parser in
  let resulting_state = eval ~state ast in
  resulting_state.output
;;
