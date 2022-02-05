type scope = { identifiers : (string, Ast.Literal.t) Hashtbl.t }

type state =
  { output : Buffer.t
  ; models : string -> Ast.Literal.t option
  ; scope : scope list
  }

let add_scope state =
  let identifiers = Hashtbl.create 10 in
  let scope = { identifiers } in
  { state with scope = scope :: state.scope }
;;

let output x state =
  let literal = Ast.Literal.to_string x in
  Buffer.add_string state.output literal
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
      state.scope |> List.find_map (fun scope -> Hashtbl.find_opt scope.identifiers id)
    in
    (match value with
    | None -> failwith "Unbound identifier" (* Unbound identifier *)
    | Some v -> v)
  | Ast.ArrayExpression expressions ->
    Ast.Literal.Array (expressions |> Iter.map (literal_of_expr state))
  | Ast.TagExpression _ -> Ast.Literal.Null (* TODO: *)
  | Ast.ForInExpression { iterator = Id ident; iterable; reverse; body } ->
    let iterable = iterable |> literal_of_expr state in
    let state = state |> add_scope in
    let maybe_rev = if reverse then Iter.rev else fun i -> i in
    (match iterable with
    | Ast.Literal.Array l ->
      let loop literal =
        state |> add_literal_to_scope ~ident ~literal;
        eval_block state body
      in
      Ast.Literal.Array (l |> maybe_rev |> Iter.map loop)
    | Ast.Literal.String s ->
      let loop c =
        let literal = Ast.Literal.String (String.make 1 c) in
        state |> add_literal_to_scope ~ident ~literal;
        eval_block state body
      in
      Ast.Literal.Array (s |> Iter.of_str |> maybe_rev |> Iter.map loop)
    | Ast.Literal.Null -> Ast.Literal.Null
    | Ast.Literal.Int _ -> failwith "Cannot iterate over int value"
    | Ast.Literal.Float _ -> failwith "Cannot iterate over float value"
    | Ast.Literal.Bool _ -> failwith "Cannot iterate over boolean value")
  | Ast.ForInRangeExpression { iterator = Id ident; reverse; from; upto; inclusive; body }
    ->
    let from = from |> literal_of_expr state in
    let upto = upto |> literal_of_expr state in
    let from, upto =
      match from, upto with
      | Ast.Literal.Int from, Ast.Literal.Int upto -> from, upto
      | Ast.Literal.Int _, _ ->
        failwith
          "Can't construct range in for loop. The end of your range is not of type int."
      | _, Ast.Literal.Int _ ->
        failwith
          "Can't construct range in for loop. The start of your range is not of type int."
      | _, _ ->
        failwith
          "Can't construct range in for loop. The start and end of your range are not of \
           type int."
    in
    let state = state |> add_scope in
    let iter =
      match reverse, from > upto with
      | true, true ->
        let start = from in
        let stop = if not inclusive then upto + 1 else upto in
        Iter.int_range_dec ~start ~stop
      | true, false -> Iter.empty
      | false, true -> Iter.empty
      | false, false ->
        let start = from in
        let stop = if not inclusive then upto - 1 else upto in
        Iter.int_range ~start ~stop
    in
    let result =
      iter
      |> Iter.map (fun i ->
             let literal = Ast.Literal.Int i in
             state |> add_literal_to_scope ~ident ~literal;
             eval_block state body)
    in
    Ast.Literal.Array result
  | Ast.TemplateExpression template_nodes ->
    Ast.Literal.String
      (template_nodes |> Iter.map (template_to_string state) |> Iter.concat_str)
  | Ast.ConditionalExpression { condition; consequent; alternate } ->
    if condition |> literal_of_expr state |> Ast.Literal.is_true
    then consequent |> literal_of_expr state
    else (
      match alternate with
      | Some alt -> alt |> literal_of_expr state
      | None -> Ast.Literal.Null)
  | Ast.UnaryExpression { operator; argument } ->
    let res = argument |> literal_of_expr state in
    (match operator, res with
    | Ast.Operators.Unary.NOT, literal -> Ast.Literal.Bool (Ast.Literal.negate literal)
    | Ast.Operators.Unary.MINUS, Ast.Literal.Int i -> Ast.Literal.Int (Int.neg i)
    | Ast.Operators.Unary.MINUS, Ast.Literal.Float f -> Ast.Literal.Float (Float.neg f)
    | Ast.Operators.Unary.MINUS, _ -> failwith "Invalid usage of unary - operator")
  | Ast.BinaryExpression { left; operator; right } ->
    let a = left |> literal_of_expr state in
    let b = right |> literal_of_expr state in
    (match operator with
    | Ast.Operators.Binary.NOT_EQUAL -> Ast.Literal.Bool (not (Ast.Literal.equal (a, b)))
    | Ast.Operators.Binary.EQUAL -> Ast.Literal.Bool (Ast.Literal.equal (a, b))
    | Ast.Operators.Binary.LESS -> Ast.Literal.Bool (Ast.Literal.compare (a, b) < 0)
    | Ast.Operators.Binary.LESS_EQUAL -> Ast.Literal.Bool (Ast.Literal.compare (a, b) <= 0)
    | Ast.Operators.Binary.GREATER -> Ast.Literal.Bool (Ast.Literal.compare (a, b) > 0)
    | Ast.Operators.Binary.GREATER_EQUAL ->
      Ast.Literal.Bool (Ast.Literal.compare (a, b) >= 0)
    | Ast.Operators.Binary.CONCAT ->
      (match a, b with
      | Ast.Literal.String a, Ast.Literal.String b -> Ast.Literal.String (a ^ b)
      | _ -> failwith "Trying to concat non string literals.")
    | Ast.Operators.Binary.PLUS ->
      (match a, b with
      | Ast.Literal.Int a, Ast.Literal.Int b -> Ast.Literal.Int (a + b)
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a +. b)
      | Ast.Literal.Float a, Ast.Literal.Int b -> Ast.Literal.Float (a +. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b -> Ast.Literal.Float (float_of_int a +. b)
      | _ -> failwith "Trying to add non numeric literals.")
    | Ast.Operators.Binary.MINUS ->
      (match a, b with
      | Ast.Literal.Int a, Ast.Literal.Int b -> Ast.Literal.Int (a - b)
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a -. b)
      | Ast.Literal.Float a, Ast.Literal.Int b -> Ast.Literal.Float (a -. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b -> Ast.Literal.Float (float_of_int a -. b)
      | _ -> failwith "Trying to subtract non numeric literals.")
    | Ast.Operators.Binary.TIMES ->
      (match a, b with
      | Ast.Literal.Int a, Ast.Literal.Int b -> Ast.Literal.Int (a * b)
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a *. b)
      | Ast.Literal.Float a, Ast.Literal.Int b -> Ast.Literal.Float (a *. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b -> Ast.Literal.Float (float_of_int a *. b)
      | _ -> failwith "Trying to multiply non numeric literals.")
    | Ast.Operators.Binary.DIV ->
      (match a, b with
      | Ast.Literal.Int a, Ast.Literal.Int b ->
        let result = float_of_int a /. float_of_int b in
        Ast.Literal.Float result
      | Ast.Literal.Float a, Ast.Literal.Float b -> Ast.Literal.Float (a /. b)
      | Ast.Literal.Float a, Ast.Literal.Int b -> Ast.Literal.Float (a /. float_of_int b)
      | Ast.Literal.Int a, Ast.Literal.Float b -> Ast.Literal.Float (float_of_int a /. b)
      | _ -> failwith "Trying to divide non numeric literals.")
    | Ast.Operators.Binary.POW ->
      (match a, b with
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
      | _ -> failwith "Trying to raise non numeric literals.")
    | Ast.Operators.Binary.AND ->
      Ast.Literal.Bool (Ast.Literal.is_true a && Ast.Literal.is_true b)
    | Ast.Operators.Binary.OR ->
      Ast.Literal.Bool (Ast.Literal.is_true a || Ast.Literal.is_true b))

and html_attr_to_string state (attr : Ast.attribute) =
  let buf = Buffer.create 64 in
  let value = literal_of_expr state attr.value |> Ast.Literal.to_string in
  let key = attr.key in
  Buffer.add_string buf key;
  Buffer.add_char buf '=';
  Buffer.add_char buf '"';
  Buffer.add_string buf value;
  Buffer.add_char buf '"';
  Buffer.contents buf

and template_to_string state template =
  match template with
  | Ast.TextTemplateNode text -> text
  | Ast.HtmlTemplateNode { tag; attributes; children; _ } ->
    let buf = Buffer.create 128 in
    Buffer.add_char buf '<';
    Buffer.add_string buf tag;
    if not (Iter.is_empty attributes)
    then (
      Buffer.add_char buf ' ';
      attributes
      |> Iter.iteri (fun i attr ->
             let res = html_attr_to_string state attr in
             if i <> 0 then Buffer.add_char buf ' ';
             Buffer.add_string buf res));
    Buffer.add_char buf '>';
    children
    |> Iter.iter (fun child ->
           let res = template_to_string state child in
           Buffer.add_string buf res);
    Buffer.add_char buf '<';
    Buffer.add_char buf '/';
    Buffer.add_string buf tag;
    Buffer.add_char buf '>';
    Buffer.contents buf
  | Ast.ExpressionTemplateNode expr -> Ast.Literal.to_string (literal_of_expr state expr)
  | Ast.ComponentTemplateNode { identifier = _; attributes = _; children = _; _ } ->
    (* TODO: *) ""

and eval_block state =
  let state = state |> add_scope in
  let result = Iter.fold (fun _acc curr -> eval_statement state curr) Ast.Literal.Null in
  result

and eval_statement state stmt =
  match stmt with
  | Ast.BreakStmt -> Ast.Literal.Null (* TODO: *)
  | Ast.ContinueStmt -> Ast.Literal.Null (* TODO: *)
  | Ast.DeclarationStmt { nullable; left = Id ident; right } ->
    let literal = literal_of_expr state right in
    (match literal with
    | Ast.Literal.Null when not nullable -> failwith "Not Nullable!!"
    | _ -> state |> add_literal_to_scope ~ident ~literal);
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

let eval ~state ast root_name =
  let declaration =
    ast
    |> Iter.find (function
           | ( Ast.ComponentDeclaration { identifier = Id name; _ }
             | Ast.SiteDeclaration { identifier = Id name; _ }
             | Ast.PageDeclaration { identifier = Id name; _ }
             | Ast.StoreDeclaration { identifier = Id name; _ } ) as declaration
             when name = root_name -> Some declaration
           | _ -> None)
  in
  match declaration with
  | Some declaration -> output (eval_declaration state declaration) state
  | None ->
    failwith (Printf.sprintf "Declaration with name `%s` was not found." root_name)
;;

let init_state ?(models = fun _ -> None) () =
  let state = { output = Buffer.create 4096; scope = []; models } in
  state
;;

let file_contents chan = really_input_string chan (in_channel_length chan)

let from_directory ?models ~directory root_name =
  let state = init_state ?models () in
  let src_match = FileUtil.Has_extension "fe" in
  let src_files = FileUtil.find src_match directory Iter.snoc Iter.empty in
  (* TODO: This should happen asynchronously *)
  let declarations = src_files |> Iter.flat_map Parser.parse_file in
  let () = eval ~state declarations root_name in
  Buffer.contents state.output
;;

let from_file ?models ~filename root_name =
  let state = init_state ?models () in
  let declarations = Parser.parse_file filename in
  let () = eval ~state declarations root_name in
  Buffer.contents state.output
;;

let from_ast ?models declarations root_name =
  let state = init_state ?models () in
  let () = eval ~state declarations root_name in
  Buffer.contents state.output
;;
