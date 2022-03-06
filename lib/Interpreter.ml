open Ast

module Value = struct
  type t =
    | Null
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Array of t Iter.t
    | Record of t StringMap.t
    | TemplateNode of [ `Component of t | `Html ] * string * t StringMap.t * t list * bool

  let rec to_string = function
    | Null -> ""
    | String s -> s
    | Int i -> string_of_int i
    | Float f when Float.is_integer f -> string_of_int (int_of_float f)
    | Float f -> string_of_float f
    | Bool b -> if b then "true" else "false"
    | Array l -> l |> Iter.map to_string |> Iter.intersperse "\n" |> Iter.concat_str
    | Record m ->
      let b = Buffer.create 1024 in
      m
      |> StringMap.iter (fun _key value ->
             Buffer.add_string b (to_string value);
             Buffer.add_char b '\n');
      Buffer.contents b
    | TemplateNode (`Html, tag, attributes, children, self_closing) ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf '<';
      Buffer.add_string buf tag;
      if not (StringMap.is_empty attributes)
      then
        attributes
        |> StringMap.iter (fun key value ->
               match value with
               | Null -> ()
               | String _ | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _
                 ->
                 Buffer.add_char buf ' ';
                 Buffer.add_string buf key;
                 Buffer.add_char buf '=';
                 Buffer.add_char buf '"';
                 Buffer.add_string buf (value |> to_string);
                 Buffer.add_char buf '"');
      if self_closing && HTML.is_void_el tag
      then Buffer.add_string buf " />"
      else (
        Buffer.add_char buf '>';
        children |> List.iter (fun child -> Buffer.add_string buf (to_string child));
        Buffer.add_char buf '<';
        Buffer.add_char buf '/';
        Buffer.add_string buf tag;
        Buffer.add_char buf '>');
      Buffer.contents buf
    | TemplateNode
        (`Component rendered_template, _tag, _attributes, _children, _self_closing) ->
      to_string rendered_template
  ;;

  let is_true = function
    | Null -> false
    | Bool b -> b
    | String s -> s |> String.trim |> String.length > 0
    | Int _ -> true
    | Float _ -> true
    | TemplateNode _ -> true
    | Array l -> not (Iter.is_empty l)
    | Record m -> not (StringMap.is_empty m)
  ;;

  let rec equal a b =
    match a, b with
    | String a, String b -> String.equal a b
    | Int a, Int b -> a = b
    | Float a, Float b -> a = b
    | Float a, Int b -> a = float_of_int b
    | Int a, Float b -> float_of_int a = b
    | Bool a, Bool b -> a = b
    | Array a, Array b -> Iter.to_rev_list a = Iter.to_rev_list b
    | Record a, Record b -> StringMap.equal equal a b
    | ( TemplateNode (a_typ, a_tag, a_attrs, a_children, a_self_closing)
      , TemplateNode (b_typ, b_tag, b_attrs, b_children, b_self_closing) ) ->
      a_typ = b_typ
      && a_tag = b_tag
      && a_self_closing = b_self_closing
      && StringMap.equal equal a_attrs b_attrs
      && a_children = b_children
    | Null, Null -> true
    | TemplateNode _, (Null | String _ | Int _ | Float _ | Bool _ | Array _ | Record _)
    | Null, (String _ | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _)
    | Array _, (Null | String _ | Int _ | Float _ | Bool _ | Record _ | TemplateNode _)
    | Bool _, (Null | String _ | Int _ | Float _ | Array _ | Record _ | TemplateNode _)
    | Float _, (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _)
    | Int _, (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _)
    | String _, (Null | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _)
    | Record _, (Null | Int _ | Float _ | Bool _ | Array _ | String _ | TemplateNode _) ->
      false
  ;;

  let rec compare a b =
    match a, b with
    | String a, String b -> String.compare a b
    | Int a, Int b -> Int.compare a b
    | Float a, Float b -> Float.compare a b
    | Float a, Int b -> Float.compare a (float_of_int b)
    | Int a, Float b -> Float.compare (float_of_int a) b
    | Bool a, Bool b -> Bool.compare a b
    | Array a, Array b -> Int.compare (Iter.length a) (Iter.length b)
    | Record a, Record b -> StringMap.compare compare a b
    | Null, Null -> 0
    | TemplateNode _, TemplateNode _ -> 0
    | TemplateNode _, (Null | String _ | Int _ | Float _ | Bool _ | Array _ | Record _) ->
      assert false
    | Null, (String _ | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _) ->
      assert false
    | Array _, (Null | String _ | Int _ | Float _ | Bool _ | Record _ | TemplateNode _) ->
      assert false
    | Bool _, (Null | String _ | Int _ | Float _ | Array _ | Record _ | TemplateNode _) ->
      assert false
    | Float _, (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _) ->
      assert false
    | Int _, (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _) ->
      assert false
    | String _, (Null | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _) ->
      assert false
    | Record _, (Null | String _ | Int _ | Float _ | Bool _ | Array _ | TemplateNode _) ->
      assert false
  ;;
end

type state =
  { output : Buffer.t
  ; binding_identifier : string option
  ; models : string -> Value.t option
  ; slotted_children : Value.t list option
  ; declarations : declaration StringMap.t
  ; declaration_instances : Value.t array StringMap.t
  ; scope : Value.t StringMap.t list
  }

let init_state
    ?(models = fun _ -> None)
    ?slotted_children
    ?(declaration_instances = StringMap.empty)
    declarations
  =
  let state =
    { output = Buffer.create 4096
    ; binding_identifier = None
    ; scope = []
    ; models
    ; slotted_children
    ; declarations
    ; declaration_instances
    }
  in
  state
;;

let add_scope state =
  let scope = StringMap.empty in
  { state with scope = scope :: state.scope }
;;

let add_value_to_scope ~ident ~value state =
  let rec update_scope state =
    match state.scope with
    | [] -> state |> add_scope |> update_scope
    | scope :: rest -> StringMap.add ident value scope :: rest
  in
  { state with scope = update_scope state }
;;

let rec eval_expression ~state = function
  | Int i -> Value.Int i
  | Float f when Float.is_integer f -> Value.Int (int_of_float f)
  | Float f -> Value.Float f
  | Bool b -> Value.Bool b
  | Array l -> Value.Array (Iter.map (eval_expression ~state) l)
  | Record map ->
    Value.Record
      (map
      |> Ast.StringMap.mapi (fun ident (optional, expression) ->
             expression
             |> eval_expression ~state:{ state with binding_identifier = Some ident }
             |> function
             | Value.Null when not optional ->
               failwith
                 (Printf.sprintf
                    "identifier %s is not marked as nullable, but was given a null value."
                    ident)
             | value -> value))
  | String template -> eval_string_template ~state template
  | LetExpression (Lowercase_Id ident, expression, body) ->
    eval_let ~state ~ident ~optional:false ~expression body
  | OptionalLetExpression (Lowercase_Id ident, expression, body) ->
    eval_let ~state ~ident ~optional:true ~expression body
  | UppercaseIdentifierExpression _ -> failwith "Not Implemented"
  | LowercaseIdentifierExpression (Lowercase_Id id) -> eval_lowercase_identifier ~state id
  | TagExpression tag -> eval_tag ~state tag
  | ForInExpression { iterator = Lowercase_Id ident; reverse; iterable; body } ->
    eval_for_in ~state ~ident ~reverse ~iterable body
  | TemplateExpression nodes ->
    Value.Array (nodes |> List.map (eval_template ~state) |> Iter.of_list)
  | BlockExpression e -> eval_block ~state e
  | ConditionalExpression { condition; consequent; alternate } ->
    eval_if ~state ~condition ~alternate consequent
  | UnaryExpression (Operators.Unary.NOT, expression) -> eval_unary_not ~state expression
  | UnaryExpression (Operators.Unary.MINUS, expression) ->
    eval_unary_minus ~state expression
  | BinaryExpression (left, Operators.Binary.EQUAL, right) ->
    eval_binary_equal ~state left right
  | BinaryExpression (left, Operators.Binary.NOT_EQUAL, right) ->
    eval_binary_not_equal ~state left right
  | BinaryExpression (left, Operators.Binary.GREATER, right) ->
    eval_binary_greater ~state left right
  | BinaryExpression (left, Operators.Binary.GREATER_EQUAL, right) ->
    eval_binary_greater_equal ~state left right
  | BinaryExpression (left, Operators.Binary.LESS, right) ->
    eval_binary_less ~state left right
  | BinaryExpression (left, Operators.Binary.LESS_EQUAL, right) ->
    eval_binary_less_equal ~state left right
  | BinaryExpression (left, Operators.Binary.PLUS, right) ->
    eval_binary_plus ~state left right
  | BinaryExpression (left, Operators.Binary.MINUS, right) ->
    eval_binary_minus ~state left right
  | BinaryExpression (left, Operators.Binary.TIMES, right) ->
    eval_binary_times ~state left right
  | BinaryExpression (left, Operators.Binary.DIV, right) ->
    eval_binary_div ~state left right
  | BinaryExpression (left, Operators.Binary.POW, right) ->
    eval_binary_pow ~state left right
  | BinaryExpression (left, Operators.Binary.MODULO, right) ->
    eval_binary_modulo ~state left right
  | BinaryExpression (left, Operators.Binary.CONCAT, right) ->
    eval_binary_concat ~state left right
  | BinaryExpression (left, Operators.Binary.AND, right) ->
    eval_binary_and ~state left right
  | BinaryExpression (left, Operators.Binary.OR, right) ->
    eval_binary_or ~state left right
  | BinaryExpression (left, Operators.Binary.DOT_ACCESS, right) ->
    eval_binary_dot_access ~state left right
  | BinaryExpression (left, Operators.Binary.BRACKET_ACCESS, right) ->
    eval_binary_bracket_access ~state left right
  | BinaryExpression (left, Operators.Binary.ARRAY_ADD, right) ->
    eval_binary_array_add ~state left right
  | BinaryExpression (left, Operators.Binary.MERGE, right) ->
    eval_binary_merge ~state left right
  | BinaryExpression (left, Operators.Binary.RANGE, right) ->
    eval_range ~state ~inclusive:false left right
  | BinaryExpression (left, Operators.Binary.INCLUSIVE_RANGE, right) ->
    eval_range ~state ~inclusive:true left right
  | BreakExpression -> failwith "Not Implemented"
  | ContinueExpression -> failwith "Not Implemented"

and eval_string_template ~state template =
  Value.String
    (template
    |> List.map (function
           | Ast.StringText s -> s
           | Ast.StringInterpolation e -> eval_expression ~state e |> Value.to_string)
    |> String.concat "")

and eval_binary_plus ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  match a, b with
  | Value.Int a, Value.Int b -> Value.Int (a + b)
  | Value.Float a, Value.Float b -> Value.Float (a +. b)
  | Value.Float a, Value.Int b -> Value.Float (a +. float_of_int b)
  | Value.Int a, Value.Float b -> Value.Float (float_of_int a +. b)
  | _ -> failwith "Trying to add non numeric literals."

and eval_binary_minus ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  match a, b with
  | Value.Int a, Value.Int b -> Value.Int (a - b)
  | Value.Float a, Value.Float b -> Value.Float (a -. b)
  | Value.Float a, Value.Int b -> Value.Float (a -. float_of_int b)
  | Value.Int a, Value.Float b -> Value.Float (float_of_int a -. b)
  | _ -> failwith "Trying to subtract non numeric literals."

and eval_binary_times ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  match a, b with
  | Value.Int a, Value.Int b -> Value.Int (a * b)
  | Value.Float a, Value.Float b -> Value.Float (a *. b)
  | Value.Float a, Value.Int b -> Value.Float (a *. float_of_int b)
  | Value.Int a, Value.Float b -> Value.Float (float_of_int a *. b)
  | _ -> failwith "Trying to multiply non numeric literals."

and eval_binary_div ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  let r =
    match a, b with
    | Value.Int _, Value.Int 0 -> failwith "Trying to divide by 0"
    | Value.Float _, Value.Float 0. -> failwith "Trying to divide by 0"
    | Value.Float _, Value.Int 0 -> failwith "Trying to divide by 0"
    | Value.Int _, Value.Float 0. -> failwith "Trying to divide by 0"
    | Value.Int a, Value.Int b -> float_of_int a /. float_of_int b
    | Value.Float a, Value.Float b -> a /. b
    | Value.Float a, Value.Int b -> a /. float_of_int b
    | Value.Int a, Value.Float b -> float_of_int a /. b
    | _ -> failwith "Trying to divide non numeric literals."
  in
  if Float.is_integer r then Value.Int (int_of_float r) else Value.Float r

and eval_binary_pow ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  let r =
    match a, b with
    | Value.Int a, Value.Int b -> float_of_int a ** float_of_int b
    | Value.Float a, Value.Float b -> a ** b
    | Value.Float a, Value.Int b -> a ** float_of_int b
    | Value.Int a, Value.Float b -> float_of_int a ** b
    | _ -> failwith "Trying to raise non numeric literals."
  in
  if Float.is_integer r then Value.Int (int_of_float r) else Value.Float r

and eval_binary_modulo ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  let ( %. ) = mod_float in
  let ( % ) = ( mod ) in
  let r =
    match a, b with
    | Value.Int _, Value.Int 0 -> failwith "Trying to modulo with 0 on right hand side."
    | Value.Int _, Value.Float 0. ->
      failwith "Trying to modulo with 0 on right hand side."
    | Value.Float _, Value.Float 0. ->
      failwith "Trying to modulo with 0 on right hand side."
    | Value.Float _, Value.Int 0 -> failwith "Trying to modulo with 0 on right hand side."
    | Value.Int a, Value.Int b -> a % b
    | Value.Float a, Value.Float b -> int_of_float (a %. b)
    | Value.Float a, Value.Int b -> int_of_float a % b
    | Value.Int a, Value.Float b -> a % int_of_float b
    | _ -> failwith "Trying to modulo non numeric literals."
  in
  Value.Int r

and eval_binary_and ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.is_true a && Value.is_true b)

and eval_binary_or ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.is_true a || Value.is_true b)

and eval_binary_less ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.compare a b < 0)

and eval_binary_less_equal ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.compare a b <= 0)

and eval_binary_greater ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.compare a b > 0)

and eval_binary_greater_equal ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.compare a b >= 0)

and eval_binary_equal ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (Value.equal a b)

and eval_binary_not_equal ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  Value.Bool (not (Value.equal a b))

and eval_binary_concat ~state left right =
  let a = left |> eval_expression ~state in
  let b = right |> eval_expression ~state in
  match a, b with
  | Value.String a, Value.String b -> Value.String (a ^ b)
  | _ -> failwith "Trying to concat non string literals."

and eval_binary_dot_access ~state left right =
  let left = left |> eval_expression ~state in
  match left, right with
  | Value.Record left, Ast.LowercaseIdentifierExpression (Lowercase_Id b) ->
    left |> StringMap.find_opt b |> Option.value ~default:Value.Null
  | Value.Record _, _ ->
    failwith "Expected right hand side of record access to be a lowercase identifier."
  | Value.Null, _ -> Value.Null
  | _, Ast.LowercaseIdentifierExpression _ ->
    failwith "Trying to access a property on a non record value."
  | _ -> failwith "I am really not sure what you are trying to do here..."

and eval_binary_bracket_access ~state left right =
  let left = left |> eval_expression ~state in
  let right = right |> eval_expression ~state in
  match left, right with
  | Value.Array left, Value.Int right ->
    left
    |> Iter.findi (fun index el -> if index = right then Some el else None)
    |> Option.value ~default:Value.Null
  | Value.Record left, Value.String right ->
    left |> StringMap.find_opt right |> Option.value ~default:Value.Null
  | Value.Null, _ -> Value.Null
  | Value.Array _, _ -> failwith "Cannot access array with a non integer value."
  | Value.Record _, _ -> failwith "Cannot access record with a non string value."
  | _ -> failwith "Trying to access a property on a non record or array value."

and eval_binary_array_add ~state left right =
  let left = left |> eval_expression ~state in
  let right = right |> eval_expression ~state in
  match left, right with
  | Value.Array left, value -> Value.Array (Iter.append left (Iter.singleton value))
  | _ -> failwith "Trying to add an element on a non array value."

and eval_binary_merge ~state left right =
  let left = left |> eval_expression ~state in
  let right = right |> eval_expression ~state in
  match left, right with
  | Value.Array left, Value.Array right -> Value.Array (Iter.append left right)
  | Value.Record left, Value.Record right ->
    Value.Record
      (StringMap.merge
         (fun _ x y ->
           match x, y with
           | (Some _ | None), Some y -> Some y
           | Some x, None -> Some x
           | None, None -> None)
         left
         right)
  | Value.Array _, _ -> failwith "Trying to merge a non array value onto an array."
  | _, Value.Array _ -> failwith "Trying to merge an array value onto a non array."
  | _ -> failwith "Trying to merge two non array values."

and eval_unary_not ~state expression =
  Value.Bool (not (Value.is_true @@ eval_expression ~state expression))

and eval_unary_minus ~state expression =
  match eval_expression ~state expression with
  | Value.Int i -> Value.Int (Int.neg i)
  | Value.Float f -> Value.Float (Float.neg f)
  | _ ->
    failwith
      "Invalid usage of unary `-` operator. You are only able to negate integers or \
       floats."

and eval_lowercase_identifier ~state id =
  state.scope
  |> List.find_map (StringMap.find_opt id)
  |> function
  | None -> failwith (Printf.sprintf "Unbound identifier `%s`" id)
  | Some v -> v

and eval_let ~state ~ident ~optional ~expression body =
  let state =
    expression
    |> eval_expression ~state:{ state with binding_identifier = Some ident }
    |> function
    | Value.Null when not optional ->
      failwith
        (Printf.sprintf
           "identifier %s is not marked as nullable, but was given a null value."
           ident)
    | value -> state |> add_value_to_scope ~ident ~value
  in
  eval_expression ~state body

and eval_if ~state ~condition ~alternate consequent =
  if condition |> eval_expression ~state |> Value.is_true
  then consequent |> eval_expression ~state
  else (
    match alternate with
    | Some alt -> alt |> eval_expression ~state
    | None -> Value.Null)

and eval_for_in ~state ~ident ~reverse ~iterable body =
  let iterable = iterable |> eval_expression ~state in
  let maybe_rev = if reverse then Iter.rev else fun i -> i in
  match iterable with
  | Value.Array l ->
    let loop value =
      let state = state |> add_value_to_scope ~ident ~value in
      eval_block ~state body
    in
    Value.Array (l |> maybe_rev |> Iter.map loop)
  | Value.String s ->
    let loop c =
      let value = Value.String (String.make 1 c) in
      let state = state |> add_value_to_scope ~ident ~value in
      eval_block ~state body
    in
    Value.Array (s |> Iter.of_str |> maybe_rev |> Iter.map loop)
  | Value.Null -> Value.Null
  | Value.TemplateNode _ -> failwith "Cannot iterate over template node"
  | Value.Record _ -> failwith "Cannot iterate over record value"
  | Value.Int _ -> failwith "Cannot iterate over int value"
  | Value.Float _ -> failwith "Cannot iterate over float value"
  | Value.Bool _ -> failwith "Cannot iterate over boolean value"

and eval_range ~state ~inclusive from upto =
  let from = from |> eval_expression ~state in
  let upto = upto |> eval_expression ~state in
  let from, upto =
    match from, upto with
    | Value.Int from, Value.Int upto -> from, upto
    | Value.Int _, _ ->
      failwith
        "Can't construct range in for loop. The end of your range is not of type int."
    | _, Value.Int _ ->
      failwith
        "Can't construct range in for loop. The start of your range is not of type int."
    | _, _ ->
      failwith
        "Can't construct range in for loop. The start and end of your range are not of \
         type int."
  in
  let iter =
    if from > upto
    then Iter.empty
    else (
      let start = from in
      let stop = if not inclusive then upto - 1 else upto in
      Iter.int_range ~start ~stop |> Iter.map (fun i -> Value.Int i))
  in
  Value.Array iter

and eval_block ~state expr =
  let state = state |> add_scope in
  eval_expression ~state expr

(* let rec literal_of_expr ?ident ~state expr =
  match expr with
  | Ast.UppercaseIdentifierExpression (Uppercase_Id id) ->
    let value =
      state.declarations
      |> Iter.find_pred (function
             | Ast.PageDeclaration { identifier = Uppercase_Id identifier; _ }
               when identifier = id -> true
             | Ast.SiteDeclaration { identifier = Uppercase_Id identifier; _ }
               when identifier = id -> true
             | Ast.ComponentDeclaration { identifier = Uppercase_Id identifier; _ }
               when identifier = id -> true
             | Ast.StoreDeclaration { identifier = Uppercase_Id identifier; _ }
               when identifier = id -> true
             | _ -> false)
    in
    (match value with
    | None ->
      failwith (Printf.sprintf "Unbound identifier `%s`" id) (* Unbound identifier *)
    | Some _v -> failwith "ok")
  
  | Ast.BinaryExpression { left; operator; right } ->
    (match operator with
    ) *)

and eval_tag ?value ~state =
  let apply_default_value ~default value =
    match default, value with
    | Some default, Value.Null -> eval_expression ~state default
    | _, value -> value
  in
  let apply_transformer ~transformer value =
    match transformer with
    | Some (Lowercase_Id ident, expr) ->
      let state = state |> add_scope |> add_value_to_scope ~ident ~value in
      eval_expression ~state expr
    | _ -> value
  in
  let get_value attributes =
    let get_key attributes =
      let key =
        StringMap.find_opt "key" attributes |> Option.map (eval_expression ~state)
      in
      match key, state.binding_identifier with
      | Some (Value.String s), _ident -> s
      | Some _, _ident -> failwith "Expected attribute `key` on tag to be of type string"
      | None, Some ident -> ident
      | None, None -> failwith "Expected attribute `key` to exist on tag"
    in
    match value with
    | Some v -> v
    | None -> state.models (get_key attributes) |> Option.value ~default:Value.Null
  in
  function
  | Ast.TagString (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let value = get_value attributes in
    (match apply_default_value ~default value with
    | Value.Int _ -> failwith "tried to assign integer value to a string tag."
    | Value.Float _ -> failwith "tried to assign float value to a string tag."
    | Value.Bool _ -> failwith "tried to assign boolean value to a string tag."
    | Value.Array _ -> failwith "tried to assign array value to a string tag."
    | Value.Record _ -> failwith "tried to assign record value to a string tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a string tag."
    | Value.Null -> Value.Null |> apply_transformer ~transformer:body
    | Value.String s -> Value.String s |> apply_transformer ~transformer:body)
  | Ast.TagInt (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let value = get_value attributes in
    (match apply_default_value ~default value with
    | Value.Float _ -> failwith "tried to assign float value to a int tag."
    | Value.Bool _ -> failwith "tried to assign boolean value to a int tag."
    | Value.Array _ -> failwith "tried to assign array value to a int tag."
    | Value.String _ -> failwith "tried to assign string value to a int tag."
    | Value.Record _ -> failwith "tried to assign record value to a int tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a int tag."
    | Value.Null -> Value.Null |> apply_transformer ~transformer:body
    | Value.Int i -> Value.Int i |> apply_transformer ~transformer:body)
  | Ast.TagFloat (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let value = get_value attributes in
    (match apply_default_value ~default value with
    | Value.Bool _ -> failwith "tried to assign boolean value to a float tag."
    | Value.Array _ -> failwith "tried to assign array value to a float tag."
    | Value.String _ -> failwith "tried to assign string value to a float tag."
    | Value.Int _ -> failwith "tried to assign int value to a float tag."
    | Value.Record _ -> failwith "tried to assign record value to a float tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a float tag."
    | Value.Null -> Value.Null |> apply_transformer ~transformer:body
    | Value.Float f -> Value.Float f |> apply_transformer ~transformer:body)
  | Ast.TagBoolean (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let value = get_value attributes in
    (match apply_default_value ~default value with
    | Value.Array _ -> failwith "tried to assign array value to a boolean tag."
    | Value.String _ -> failwith "tried to assign string value to a boolean tag."
    | Value.Int _ -> failwith "tried to assign int value to a boolean tag."
    | Value.Float _ -> failwith "tried to assign float value to a boolean tag."
    | Value.Record _ -> failwith "tried to assign record value to a boolean tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a boolean tag."
    | Value.Null -> Value.Null |> apply_transformer ~transformer:body
    | Value.Bool b -> Value.Bool b |> apply_transformer ~transformer:body)
  | Ast.TagArray (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let of' =
      StringMap.find_opt "of" attributes
      |> Option.map (function
             | Ast.TagExpression t -> t
             | _ ->
               failwith
                 "Expected attribute `of` to on #Array to be a tag, describing the type \
                  of the items of the array.")
      |> function
      | None -> failwith "Expected attribute `of` to be present on #Array."
      | Some t -> t
    in
    let value = get_value attributes in
    (match apply_default_value ~default value with
    | Value.Bool _ -> failwith "tried to assign boolean value to a array tag."
    | Value.String _ -> failwith "tried to assign string value to a array tag."
    | Value.Int _ -> failwith "tried to assign int value to a array tag."
    | Value.Float _ -> failwith "tried to assign float value to a array tag."
    | Value.Record _ -> failwith "tried to assign record value to a array tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a array tag."
    | Value.Null -> Value.Null
    | Value.Array l ->
      let eval_item item = of' |> eval_tag ~value:item ~state in
      Value.Array (Iter.map eval_item l) |> apply_transformer ~transformer:body)
  | Ast.TagRecord (attributes, body) ->
    let of' =
      StringMap.find_opt "of" attributes
      |> Option.map (function
             | Ast.Record t -> t
             | _ ->
               failwith
                 "Expected attribute `of` to on #Record to be a record, describing the \
                  shape the items inside.")
      |> function
      | None -> failwith "Expected attribute `of` to be present on #Record."
      | Some t -> t
    in
    let value = get_value attributes in
    (match value with
    | Value.Bool _ -> failwith "tried to assign boolean value to a record tag."
    | Value.String _ -> failwith "tried to assign string value to a record tag."
    | Value.Int _ -> failwith "tried to assign int value to a record tag."
    | Value.Float _ -> failwith "tried to assign float value to a record tag."
    | Value.Array _ -> failwith "tried to assign array value to a record tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a record tag."
    | Value.Null -> Value.Null
    | Value.Record r ->
      let models key = StringMap.find_opt key r in
      let state = init_state ~models state.declarations in
      let eval_property key (nullable, expr) =
        match expr with
        | Ast.TagExpression tag ->
          tag
          |> eval_tag ~state:{ state with binding_identifier = Some key }
          |> (function
          | Value.Null when not nullable ->
            failwith
              (Printf.sprintf
                 "property `%s` on record tag is not marked as nullable, but was given a \
                  null value."
                 key)
          | value -> value)
        | _ ->
          failwith
            (Printf.sprintf
               "expected property `%s` on record tag to be a tag, describing the type of \
                the property."
               key)
      in
      let r = of' |> StringMap.mapi eval_property in
      Value.Record r |> apply_transformer ~transformer:body)
  | Ast.TagSlot attributes ->
    let slot_name =
      attributes
      |> StringMap.find_opt "name"
      |> Option.map (eval_expression ~state)
      |> Option.value ~default:(Value.String "")
      |> function
      | Value.String s -> s
      | _ -> failwith "Expected attribute `name` on #Slot to be of type string."
    in
    (match state.slotted_children with
    | None -> Value.Null
    | Some children ->
      let find_slot_key attributes =
        attributes
        |> StringMap.find_opt "slot"
        |> Option.value ~default:(Value.String "")
        |> function
        | Value.String s -> s
        | _ -> failwith "Expected slot attribute to be of type string"
      in
      let rec keep_slotted acc = function
        | (Value.TemplateNode (`Html, _tag, attributes, _children, _self_closing) as
          value)
        | Value.TemplateNode (`Component value, _tag, attributes, _children, _self_closing)
          ->
          if find_slot_key attributes = slot_name
          then Iter.append acc (Iter.singleton value)
          else acc
        | Value.Array l -> l |> Iter.fold keep_slotted acc
        | value when slot_name = "" -> Iter.append acc (Iter.singleton value)
        | _ -> acc
      in
      let slotted_children =
        children |> Iter.of_list |> Iter.fold keep_slotted Iter.empty
      in
      Value.Array slotted_children)

and eval_template ~state template =
  match template with
  | Ast.TextTemplateNode text -> Value.String text
  | Ast.HtmlTemplateNode { tag; attributes; children; self_closing } ->
    let attributes = attributes |> StringMap.map (eval_expression ~state) in
    let children = children |> List.map (eval_template ~state) in
    Value.TemplateNode (`Html, tag, attributes, children, self_closing)
  | Ast.ExpressionTemplateNode expr -> eval_expression ~state expr
  | Ast.ComponentTemplateNode { identifier = Uppercase_Id tag; attributes; children } ->
    let attributes = attributes |> StringMap.map (eval_expression ~state) in
    let children = children |> List.map (eval_template ~state) in
    let models s = attributes |> StringMap.find_opt s in
    let state = init_state ~models ~slotted_children:children state.declarations in
    let value = eval ~root:tag ~state in
    Value.TemplateNode (`Component value, tag, attributes, children, false)

and eval_declaration ~state declaration =
  match declaration with
  | Ast.ComponentDeclaration (_attrs, body) -> eval_expression ~state body
  | Ast.SiteDeclaration (_attrs, body) -> eval_expression ~state body
  | Ast.PageDeclaration (_attrs, body) -> eval_expression ~state body
  | Ast.StoreDeclaration (_attrs, body) -> eval_expression ~state body

and eval ~state ~root =
  state.declarations
  |> StringMap.find_opt root
  |> function
  | Some declaration -> eval_declaration ~state declaration
  | None -> failwith (Printf.sprintf "Declaration with name `%s` was not found." root)
;;

let file_contents chan = really_input_string chan (in_channel_length chan)

let from_directory ?models ~directory root =
  let src_match = FileUtil.Has_extension "pi" in
  let src_files = FileUtil.find src_match directory Iter.snoc Iter.empty in
  (* TODO: This should happen asynchronously *)
  let declarations =
    src_files
    |> Iter.fold
         (fun acc curr ->
           let decls = Parser.parse_file curr in
           let f key x y =
             match x, y with
             | None, Some y -> Some y
             | Some x, None -> Some x
             | Some _, Some _ ->
               failwith
                 (Printf.sprintf "Found multiple declarations with identifier %s" key)
             | None, None -> None
           in
           StringMap.merge f acc decls)
         StringMap.empty
  in
  let state = init_state ?models declarations in
  eval ~state ~root |> Value.to_string
;;

let from_file ?models ~filename root =
  let declarations = Parser.parse_file filename in
  let state = init_state ?models declarations in
  eval ~state ~root |> Value.to_string
;;

let from_ast ?models declarations root =
  let state = init_state ?models declarations in
  eval ~state ~root |> Value.to_string
;;
