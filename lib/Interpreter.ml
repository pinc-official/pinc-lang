module StringMap = Ast.StringMap

module Value = struct
  type definition_info =
    { name : string
    ; exists : bool
    ; negated : bool
    }

  type t =
    | Null
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Array of t Iter.t
    | Record of t StringMap.t
    | DefinitionInfo of definition_info
    | TemplateNode of
        [ `Component of models:(string -> t option) -> slotted_children:t list -> t
        | `Html
        ]
        * string
        * t StringMap.t
        * t list
        * bool

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
               | String _
               | Int _
               | Float _
               | Bool _
               | Array _
               | Record _
               | TemplateNode _
               | DefinitionInfo _ ->
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
        (`Component render_fn, _tag, attributes, slotted_children, _self_closing) ->
      let models s = attributes |> StringMap.find_opt s in
      render_fn ~models ~slotted_children |> to_string
    | DefinitionInfo _ -> ""
  ;;

  let is_true = function
    | Null -> false
    | Bool b -> b
    | String s -> s |> String.trim |> String.length > 0
    | Int _ -> true
    | Float _ -> true
    | TemplateNode _ -> true
    | DefinitionInfo { exists; _ } -> exists
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
    | DefinitionInfo { name = a; _ }, DefinitionInfo { name = b; _ } -> String.equal a b
    | ( TemplateNode (a_typ, a_tag, a_attrs, a_children, a_self_closing)
      , TemplateNode (b_typ, b_tag, b_attrs, b_children, b_self_closing) ) ->
      a_typ = b_typ
      && a_tag = b_tag
      && a_self_closing = b_self_closing
      && StringMap.equal equal a_attrs b_attrs
      && a_children = b_children
    | Null, Null -> true
    | ( DefinitionInfo _
      , (Null | String _ | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _)
      )
    | ( TemplateNode _
      , ( Null
        | String _
        | Int _
        | Float _
        | Bool _
        | Array _
        | Record _
        | DefinitionInfo _ ) )
    | ( Null
      , ( String _
        | Int _
        | Float _
        | Bool _
        | Array _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) )
    | ( Array _
      , ( Null
        | String _
        | Int _
        | Float _
        | Bool _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) )
    | ( Bool _
      , ( Null
        | String _
        | Int _
        | Float _
        | Array _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) )
    | ( Float _
      , (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _ | DefinitionInfo _)
      )
    | ( Int _
      , (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _ | DefinitionInfo _)
      )
    | ( String _
      , ( Null
        | Int _
        | Float _
        | Bool _
        | Array _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) )
    | ( Record _
      , ( Null
        | Int _
        | Float _
        | Bool _
        | Array _
        | String _
        | TemplateNode _
        | DefinitionInfo _ ) ) -> false
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
    | DefinitionInfo _, DefinitionInfo _ -> 0
    | ( DefinitionInfo _
      , (Null | String _ | Int _ | Float _ | Bool _ | Array _ | Record _ | TemplateNode _)
      ) -> assert false
    | ( TemplateNode _
      , ( Null
        | String _
        | Int _
        | Float _
        | Bool _
        | Array _
        | Record _
        | DefinitionInfo _ ) ) -> assert false
    | ( Null
      , ( String _
        | Int _
        | Float _
        | Bool _
        | Array _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) ) -> assert false
    | ( Array _
      , ( Null
        | String _
        | Int _
        | Float _
        | Bool _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) ) -> assert false
    | ( Bool _
      , ( Null
        | String _
        | Int _
        | Float _
        | Array _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) ) -> assert false
    | ( Float _
      , (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _ | DefinitionInfo _)
      ) -> assert false
    | ( Int _
      , (Null | String _ | Bool _ | Array _ | Record _ | TemplateNode _ | DefinitionInfo _)
      ) -> assert false
    | ( String _
      , ( Null
        | Int _
        | Float _
        | Bool _
        | Array _
        | Record _
        | TemplateNode _
        | DefinitionInfo _ ) ) -> assert false
    | ( Record _
      , ( Null
        | String _
        | Int _
        | Float _
        | Bool _
        | Array _
        | TemplateNode _
        | DefinitionInfo _ ) ) -> assert false
  ;;
end

module Output = struct
  type t =
    { value : Value.t option
    ; top_level_bindings : Value.t StringMap.t
    ; tags : tag_meta StringMap.t
    }

  and tag_meta = { typ : tag_typ }

  and tag_typ =
    | String
    | Int
    | Float
    | Boolean
    | Array of tag_typ
    | Record of (bool * tag_typ) StringMap.t
    | Slot

  let empty =
    { value = None; top_level_bindings = StringMap.empty; tags = StringMap.empty }
  ;;

  let get_value t =
    match t.value with
    | Some v -> v
    | None -> Value.Null
  ;;

  let add_value v t = { t with value = Some v }
  let get_bindings t = t.top_level_bindings |> StringMap.bindings

  let add_binding (key, value) t =
    { t with top_level_bindings = StringMap.add key value t.top_level_bindings }
  ;;

  let get_tags t = t.tags |> StringMap.bindings
  let print_tags t = t.tags |> StringMap.iter (fun key _ -> print_endline key)

  let add_tag ~typ key t =
    let value = { typ } in
    { t with tags = StringMap.add key value t.tags }
  ;;
end

module State = struct
  type t =
    { binding_identifier : string option
    ; models : string -> Value.t option
    ; slotted_children : Value.t list option
    ; declarations : Ast.declaration StringMap.t
    ; declaration_instances : Value.t array StringMap.t
    ; scope : Value.t StringMap.t list
    }

  let make
      ?(models = fun _ -> None)
      ?slotted_children
      ?(declaration_instances = StringMap.empty)
      declarations
    =
    { binding_identifier = None
    ; scope = []
    ; models
    ; slotted_children
    ; declarations
    ; declaration_instances
    }
  ;;

  let add_scope t =
    let scope = StringMap.empty in
    { t with scope = scope :: t.scope }
  ;;

  let add_value_to_scope ~ident ~value t =
    let update_scope t =
      match t.scope with
      | [] -> assert false
      | scope :: rest -> StringMap.add ident value scope :: rest
    in
    { t with scope = update_scope t }
  ;;
end

let rec eval_expression ~output ~state = function
  | Ast.Int i -> output |> Output.add_value (Value.Int i)
  | Ast.Float f when Float.is_integer f ->
    output |> Output.add_value (Value.Int (int_of_float f))
  | Ast.Float f -> output |> Output.add_value (Value.Float f)
  | Ast.Bool b -> output |> Output.add_value (Value.Bool b)
  | Ast.Array l ->
    output
    |> Output.add_value
         (Value.Array
            (l |> Iter.map (eval_expression ~output ~state) |> Iter.map Output.get_value))
  | Ast.Record map ->
    output
    |> Output.add_value
         (Value.Record
            (map
            |> Ast.StringMap.mapi (fun ident (optional, expression) ->
                   expression
                   |> eval_expression
                        ~output
                        ~state:{ state with State.binding_identifier = Some ident }
                   |> Output.get_value
                   |> function
                   | Value.Null when not optional ->
                     failwith
                       (Printf.sprintf
                          "identifier %s is not marked as nullable, but was given a null \
                           value."
                          ident)
                   | value -> value)))
  | Ast.String template -> eval_string_template ~output ~state template
  | Ast.LetExpression (Lowercase_Id ident, expression, body) ->
    eval_let ~output ~state ~ident ~optional:false ~expression body
  | Ast.OptionalLetExpression (Lowercase_Id ident, expression, body) ->
    eval_let ~output ~state ~ident ~optional:true ~expression body
  | Ast.UppercaseIdentifierExpression (Uppercase_Id id) ->
    let value = state.State.declarations |> StringMap.find_opt id in
    output
    |> Output.add_value
         (Value.DefinitionInfo
            { name = id; exists = Option.is_some value; negated = false })
  | Ast.LowercaseIdentifierExpression (Lowercase_Id id) ->
    eval_lowercase_identifier ~output ~state id
  | Ast.TagExpression tag -> eval_tag ~output ~state tag
  | Ast.ForInExpression { iterator = Lowercase_Id ident; reverse; iterable; body } ->
    eval_for_in ~output ~state ~ident ~reverse ~iterable body
  | Ast.TemplateExpression nodes ->
    output
    |> Output.add_value
         (Value.Array
            (nodes
            |> List.map (eval_template ~output ~state)
            |> List.map Output.get_value
            |> Iter.of_list))
  | Ast.BlockExpression e -> eval_block ~output ~state e
  | Ast.ConditionalExpression { condition; consequent; alternate } ->
    eval_if ~output ~state ~condition ~alternate consequent
  | Ast.UnaryExpression (Ast.Operators.Unary.NOT, expression) ->
    eval_unary_not ~output ~state expression
  | Ast.UnaryExpression (Ast.Operators.Unary.MINUS, expression) ->
    eval_unary_minus ~output ~state expression
  | Ast.BinaryExpression (left, Ast.Operators.Binary.EQUAL, right) ->
    eval_binary_equal ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.NOT_EQUAL, right) ->
    eval_binary_not_equal ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.GREATER, right) ->
    eval_binary_greater ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.GREATER_EQUAL, right) ->
    eval_binary_greater_equal ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.LESS, right) ->
    eval_binary_less ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.LESS_EQUAL, right) ->
    eval_binary_less_equal ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.PLUS, right) ->
    eval_binary_plus ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MINUS, right) ->
    eval_binary_minus ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.TIMES, right) ->
    eval_binary_times ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.DIV, right) ->
    eval_binary_div ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.POW, right) ->
    eval_binary_pow ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MODULO, right) ->
    eval_binary_modulo ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.CONCAT, right) ->
    eval_binary_concat ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.AND, right) ->
    eval_binary_and ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.OR, right) ->
    eval_binary_or ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.DOT_ACCESS, right) ->
    eval_binary_dot_access ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.BRACKET_ACCESS, right) ->
    eval_binary_bracket_access ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.ARRAY_ADD, right) ->
    eval_binary_array_add ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MERGE, right) ->
    eval_binary_merge ~output ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.RANGE, right) ->
    eval_range ~output ~state ~inclusive:false left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.INCLUSIVE_RANGE, right) ->
    eval_range ~output ~state ~inclusive:true left right
  | Ast.BreakExpression -> failwith "Not Implemented"
  | Ast.ContinueExpression -> failwith "Not Implemented"

and eval_string_template ~output ~state template =
  output
  |> Output.add_value
       (Value.String
          (template
          |> List.map (function
                 | Ast.StringText s -> s
                 | Ast.StringInterpolation e ->
                   eval_expression ~output ~state e |> Output.get_value |> Value.to_string)
          |> String.concat ""))

and eval_binary_plus ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  match a, b with
  | Value.Int a, Value.Int b -> output |> Output.add_value (Value.Int (a + b))
  | Value.Float a, Value.Float b -> output |> Output.add_value (Value.Float (a +. b))
  | Value.Float a, Value.Int b ->
    output |> Output.add_value (Value.Float (a +. float_of_int b))
  | Value.Int a, Value.Float b ->
    output |> Output.add_value (Value.Float (float_of_int a +. b))
  | _ -> failwith "Trying to add non numeric literals."

and eval_binary_minus ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  match a, b with
  | Value.Int a, Value.Int b -> output |> Output.add_value (Value.Int (a - b))
  | Value.Float a, Value.Float b -> output |> Output.add_value (Value.Float (a -. b))
  | Value.Float a, Value.Int b ->
    output |> Output.add_value (Value.Float (a -. float_of_int b))
  | Value.Int a, Value.Float b ->
    output |> Output.add_value (Value.Float (float_of_int a -. b))
  | _ -> failwith "Trying to subtract non numeric literals."

and eval_binary_times ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  match a, b with
  | Value.Int a, Value.Int b -> output |> Output.add_value (Value.Int (a * b))
  | Value.Float a, Value.Float b -> output |> Output.add_value (Value.Float (a *. b))
  | Value.Float a, Value.Int b ->
    output |> Output.add_value (Value.Float (a *. float_of_int b))
  | Value.Int a, Value.Float b ->
    output |> Output.add_value (Value.Float (float_of_int a *. b))
  | _ -> failwith "Trying to multiply non numeric literals."

and eval_binary_div ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
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
  if Float.is_integer r
  then output |> Output.add_value (Value.Int (int_of_float r))
  else output |> Output.add_value (Value.Float r)

and eval_binary_pow ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  let r =
    match a, b with
    | Value.Int a, Value.Int b -> float_of_int a ** float_of_int b
    | Value.Float a, Value.Float b -> a ** b
    | Value.Float a, Value.Int b -> a ** float_of_int b
    | Value.Int a, Value.Float b -> float_of_int a ** b
    | _ -> failwith "Trying to raise non numeric literals."
  in
  if Float.is_integer r
  then output |> Output.add_value (Value.Int (int_of_float r))
  else output |> Output.add_value (Value.Float r)

and eval_binary_modulo ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
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
  output |> Output.add_value (Value.Int r)

and eval_binary_and ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.is_true a && Value.is_true b))

and eval_binary_or ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.is_true a || Value.is_true b))

and eval_binary_less ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.compare a b < 0))

and eval_binary_less_equal ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.compare a b <= 0))

and eval_binary_greater ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.compare a b > 0))

and eval_binary_greater_equal ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.compare a b >= 0))

and eval_binary_equal ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (Value.equal a b))

and eval_binary_not_equal ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  output |> Output.add_value (Value.Bool (not (Value.equal a b)))

and eval_binary_concat ~output ~state left right =
  let a = left |> eval_expression ~output ~state |> Output.get_value in
  let b = right |> eval_expression ~output ~state |> Output.get_value in
  match a, b with
  | Value.String a, Value.String b -> output |> Output.add_value (Value.String (a ^ b))
  | _ -> failwith "Trying to concat non string literals."

and eval_binary_dot_access ~output ~state left right =
  let left = left |> eval_expression ~output ~state |> Output.get_value in
  match left, right with
  | Value.Record left, Ast.LowercaseIdentifierExpression (Lowercase_Id b) ->
    let value = left |> StringMap.find_opt b |> Option.value ~default:Value.Null in
    output |> Output.add_value value
  | ( Value.TemplateNode (_typ, tag, attributes, children, _self_closing)
    , Ast.LowercaseIdentifierExpression (Lowercase_Id b) ) ->
    (match b with
    | "tag" -> output |> Output.add_value (Value.String tag)
    | "attributes" -> output |> Output.add_value (Value.Record attributes)
    | "children" -> output |> Output.add_value (Value.Array (Iter.of_list children))
    | s ->
      failwith
        (Printf.sprintf
           "Unknown property %s on template node. Known properties are: `tag`, \
            `attributes` and `children`."
           s))
  | Value.Record _, _ ->
    failwith "Expected right hand side of record access to be a lowercase identifier."
  | Value.Null, _ -> output |> Output.add_value Value.Null
  | _, Ast.LowercaseIdentifierExpression _ ->
    failwith "Trying to access a property on a non record or template value."
  | _ -> failwith "I am really not sure what you are trying to do here..."

and eval_binary_bracket_access ~output ~state left right =
  let left = left |> eval_expression ~output ~state |> Output.get_value in
  let right = right |> eval_expression ~output ~state |> Output.get_value in
  match left, right with
  | Value.Array left, Value.Int right ->
    let value =
      left
      |> Iter.findi (fun index el -> if index = right then Some el else None)
      |> Option.value ~default:Value.Null
    in
    output |> Output.add_value value
  | Value.Record left, Value.String right ->
    let value = left |> StringMap.find_opt right |> Option.value ~default:Value.Null in
    output |> Output.add_value value
  | Value.Null, _ -> output |> Output.add_value Value.Null
  | Value.Array _, _ -> failwith "Cannot access array with a non integer value."
  | Value.Record _, _ -> failwith "Cannot access record with a non string value."
  | _ -> failwith "Trying to access a property on a non record or array value."

and eval_binary_array_add ~output ~state left right =
  let left = left |> eval_expression ~output ~state |> Output.get_value in
  let right = right |> eval_expression ~output ~state |> Output.get_value in
  match left, right with
  | Value.Array left, value ->
    output |> Output.add_value (Value.Array (Iter.append left (Iter.singleton value)))
  | _ -> failwith "Trying to add an element on a non array value."

and eval_binary_merge ~output ~state left right =
  let left = left |> eval_expression ~output ~state |> Output.get_value in
  let right = right |> eval_expression ~output ~state |> Output.get_value in
  match left, right with
  | Value.Array left, Value.Array right ->
    output |> Output.add_value (Value.Array (Iter.append left right))
  | Value.Record left, Value.Record right ->
    output
    |> Output.add_value
         (Value.Record
            (StringMap.merge
               (fun _ x y ->
                 match x, y with
                 | (Some _ | None), Some y -> Some y
                 | Some x, None -> Some x
                 | None, None -> None)
               left
               right))
  | Value.TemplateNode (typ, tag, attributes, children, self_closing), Value.Record right
    ->
    let attributes =
      StringMap.merge
        (fun _ x y ->
          match x, y with
          | (Some _ | None), Some y -> Some y
          | Some x, None -> Some x
          | None, None -> None)
        attributes
        right
    in
    output
    |> Output.add_value
         (Value.TemplateNode (typ, tag, attributes, children, self_closing))
  | Value.TemplateNode _, _ ->
    failwith "Trying to merge a non record value onto tag attributes."
  | Value.Array _, _ -> failwith "Trying to merge a non array value onto an array."
  | _, Value.Array _ -> failwith "Trying to merge an array value onto a non array."
  | _ -> failwith "Trying to merge two non array values."

and eval_unary_not ~output ~state expression =
  match eval_expression ~output ~state expression |> Output.get_value with
  | Value.DefinitionInfo info ->
    output
    |> Output.add_value (Value.DefinitionInfo { info with negated = not info.negated })
  | v -> output |> Output.add_value (Value.Bool (not (Value.is_true v)))

and eval_unary_minus ~output ~state expression =
  match eval_expression ~output ~state expression |> Output.get_value with
  | Value.Int i -> output |> Output.add_value (Value.Int (Int.neg i))
  | Value.Float f -> output |> Output.add_value (Value.Float (Float.neg f))
  | _ ->
    failwith
      "Invalid usage of unary `-` operator. You are only able to negate integers or \
       floats."

and eval_lowercase_identifier ~output ~state id =
  state.State.scope
  |> List.find_map (StringMap.find_opt id)
  |> function
  | None -> failwith (Printf.sprintf "Unbound identifier `%s`" id)
  | Some v -> output |> Output.add_value v

and eval_let ~output ~state ~ident ~optional ~expression body =
  let output, state =
    let output =
      expression
      |> eval_expression
           ~output
           ~state:{ state with State.binding_identifier = Some ident }
    in
    output
    |> Output.get_value
    |> function
    | Value.Null when not optional ->
      failwith
        (Printf.sprintf
           "identifier %s is not marked as nullable, but was given a null value."
           ident)
    | value ->
      let output =
        match state.State.scope with
        | [ _ ] -> output |> Output.add_binding (ident, value)
        | _ -> output
      in
      let state = state |> State.add_value_to_scope ~ident ~value in
      output, state
  in
  eval_expression ~output ~state body

and eval_if ~output ~state ~condition ~alternate consequent =
  if condition |> eval_expression ~output ~state |> Output.get_value |> Value.is_true
  then consequent |> eval_expression ~output ~state
  else (
    match alternate with
    | Some alt -> alt |> eval_expression ~output ~state
    | None -> output |> Output.add_value Value.Null)

and eval_for_in ~output ~state ~ident ~reverse ~iterable body =
  let iterable = iterable |> eval_expression ~output ~state |> Output.get_value in
  let maybe_rev = if reverse then Iter.rev else fun i -> i in
  match iterable with
  | Value.Array l ->
    let loop value =
      let state = state |> State.add_value_to_scope ~ident ~value in
      eval_block ~output ~state body
    in
    output
    |> Output.add_value
         (Value.Array (l |> maybe_rev |> Iter.map loop |> Iter.map Output.get_value))
  | Value.String s ->
    let loop c =
      let value = Value.String (String.make 1 c) in
      let state = state |> State.add_value_to_scope ~ident ~value in
      eval_block ~output ~state body
    in
    output
    |> Output.add_value
         (Value.Array
            (s |> Iter.of_str |> maybe_rev |> Iter.map loop |> Iter.map Output.get_value))
  | Value.Null -> output |> Output.add_value Value.Null
  | Value.TemplateNode _ -> failwith "Cannot iterate over template node"
  | Value.Record _ -> failwith "Cannot iterate over record value"
  | Value.Int _ -> failwith "Cannot iterate over int value"
  | Value.Float _ -> failwith "Cannot iterate over float value"
  | Value.Bool _ -> failwith "Cannot iterate over boolean value"
  | Value.DefinitionInfo _ -> failwith "Cannot iterate over definition info"

and eval_range ~output ~state ~inclusive from upto =
  let from = from |> eval_expression ~output ~state |> Output.get_value in
  let upto = upto |> eval_expression ~output ~state |> Output.get_value in
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
  output |> Output.add_value (Value.Array iter)

and eval_block ~output ~state expr =
  let state = state |> State.add_scope in
  eval_expression ~output ~state expr

and eval_tag ~output ?value ~state tag =
  let apply_default_value ~default value =
    match default, value with
    | Some default, Value.Null ->
      eval_expression ~output ~state default |> Output.get_value
    | _, value -> value
  in
  let apply_transformer ~transformer value =
    match transformer with
    | Some (Ast.Lowercase_Id ident, expr) ->
      let state = state |> State.add_scope |> State.add_value_to_scope ~ident ~value in
      eval_expression ~output ~state expr |> Output.get_value
    | _ -> value
  in
  let get_key attributes =
    let key =
      StringMap.find_opt "key" attributes
      |> Option.map (eval_expression ~output ~state)
      |> Option.map Output.get_value
    in
    match key, state.binding_identifier with
    | Some (Value.String s), _ident -> s
    | Some _, _ident -> failwith "Expected attribute `key` on tag to be of type string"
    | None, Some ident -> ident
    | None, None -> failwith "Expected attribute `key` to exist on tag"
  in
  let get_value key =
    match value with
    | Some v -> v
    | None -> state.models key |> Option.value ~default:Value.Null
  in
  let rec get_output_typ tag =
    match tag with
    | Ast.TagString (_, _) -> Output.String
    | Ast.TagInt (_, _) -> Output.Int
    | Ast.TagFloat (_, _) -> Output.Float
    | Ast.TagBoolean (_, _) -> Output.Boolean
    | Ast.TagSlot (_, _) -> Output.Slot
    | Ast.TagArray (attributes, _) ->
      let of' =
        StringMap.find_opt "of" attributes
        |> Option.map (function
               | Ast.TagExpression t -> t
               | _ ->
                 failwith
                   "Expected attribute `of` to on #Array to be a tag, describing the \
                    type of the items of the array.")
        |> function
        | None -> failwith "Expected attribute `of` to be present on #Array."
        | Some t -> t |> get_output_typ
      in
      Output.Array of'
    | Ast.TagRecord (attributes, _) ->
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
        | Some t ->
          t
          |> StringMap.mapi (fun key (nullable, expr) ->
                 match expr with
                 | Ast.TagExpression tag -> nullable, get_output_typ tag
                 | _ ->
                   failwith
                     (Printf.sprintf
                        "expected property `%s` on record tag to be a tag, describing \
                         the type of the property."
                        key))
      in
      Output.Record of'
  in
  match tag with
  | Ast.TagString (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let key = get_key attributes in
    let value = get_value key in
    let output = output |> Output.add_tag ~typ:(get_output_typ tag) key in
    (match apply_default_value ~default value with
    | Value.Int _ -> failwith "tried to assign integer value to a string tag."
    | Value.Float _ -> failwith "tried to assign float value to a string tag."
    | Value.Bool _ -> failwith "tried to assign boolean value to a string tag."
    | Value.Array _ -> failwith "tried to assign array value to a string tag."
    | Value.Record _ -> failwith "tried to assign record value to a string tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a string tag."
    | Value.DefinitionInfo _ ->
      failwith "tried to assign definition info to a string tag."
    | Value.Null ->
      output |> Output.add_value (Value.Null |> apply_transformer ~transformer:body)
    | Value.String s ->
      output |> Output.add_value (Value.String s |> apply_transformer ~transformer:body))
  | Ast.TagInt (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let key = get_key attributes in
    let value = get_value key in
    let output = output |> Output.add_tag ~typ:(get_output_typ tag) key in
    (match apply_default_value ~default value with
    | Value.Float _ -> failwith "tried to assign float value to a int tag."
    | Value.Bool _ -> failwith "tried to assign boolean value to a int tag."
    | Value.Array _ -> failwith "tried to assign array value to a int tag."
    | Value.String _ -> failwith "tried to assign string value to a int tag."
    | Value.Record _ -> failwith "tried to assign record value to a int tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a int tag."
    | Value.DefinitionInfo _ -> failwith "tried to assign definition info to a int tag."
    | Value.Null ->
      output |> Output.add_value (Value.Null |> apply_transformer ~transformer:body)
    | Value.Int i ->
      output |> Output.add_value (Value.Int i |> apply_transformer ~transformer:body))
  | Ast.TagFloat (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let key = get_key attributes in
    let value = get_value key in
    let output = output |> Output.add_tag ~typ:(get_output_typ tag) key in
    (match apply_default_value ~default value with
    | Value.Bool _ -> failwith "tried to assign boolean value to a float tag."
    | Value.Array _ -> failwith "tried to assign array value to a float tag."
    | Value.String _ -> failwith "tried to assign string value to a float tag."
    | Value.Int _ -> failwith "tried to assign int value to a float tag."
    | Value.Record _ -> failwith "tried to assign record value to a float tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a float tag."
    | Value.DefinitionInfo _ -> failwith "tried to assign definition info to a float tag."
    | Value.Null ->
      output |> Output.add_value (Value.Null |> apply_transformer ~transformer:body)
    | Value.Float f ->
      output |> Output.add_value (Value.Float f |> apply_transformer ~transformer:body))
  | Ast.TagBoolean (attributes, body) ->
    let default = StringMap.find_opt "default" attributes in
    let key = get_key attributes in
    let value = get_value key in
    let output = output |> Output.add_tag ~typ:(get_output_typ tag) key in
    (match apply_default_value ~default value with
    | Value.Array _ -> failwith "tried to assign array value to a boolean tag."
    | Value.String _ -> failwith "tried to assign string value to a boolean tag."
    | Value.Int _ -> failwith "tried to assign int value to a boolean tag."
    | Value.Float _ -> failwith "tried to assign float value to a boolean tag."
    | Value.Record _ -> failwith "tried to assign record value to a boolean tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a boolean tag."
    | Value.DefinitionInfo _ ->
      failwith "tried to assign definition info to a boolean tag."
    | Value.Null ->
      output |> Output.add_value (Value.Null |> apply_transformer ~transformer:body)
    | Value.Bool b ->
      output |> Output.add_value (Value.Bool b |> apply_transformer ~transformer:body))
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
    let key = get_key attributes in
    let value = get_value key in
    let output = output |> Output.add_tag ~typ:(get_output_typ tag) key in
    (match apply_default_value ~default value with
    | Value.Bool _ -> failwith "tried to assign boolean value to a array tag."
    | Value.String _ -> failwith "tried to assign string value to a array tag."
    | Value.Int _ -> failwith "tried to assign int value to a array tag."
    | Value.Float _ -> failwith "tried to assign float value to a array tag."
    | Value.Record _ -> failwith "tried to assign record value to a array tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a array tag."
    | Value.DefinitionInfo _ -> failwith "tried to assign definition info to a array tag."
    | Value.Null -> output |> Output.add_value Value.Null
    | Value.Array l ->
      let eval_item item = of' |> eval_tag ~value:item ~output ~state in
      let value =
        Value.Array (Iter.map eval_item l |> Iter.map Output.get_value)
        |> apply_transformer ~transformer:body
      in
      output |> Output.add_value value)
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
      | Some t ->
        t
        |> StringMap.mapi (fun key (nullable, expr) ->
               match expr with
               | Ast.TagExpression tag -> nullable, tag
               | _ ->
                 failwith
                   (Printf.sprintf
                      "expected property `%s` on record tag to be a tag, describing the \
                       type of the property."
                      key))
    in
    let key = get_key attributes in
    let value = get_value key in
    let output = output |> Output.add_tag ~typ:(get_output_typ tag) key in
    (match value with
    | Value.Bool _ -> failwith "tried to assign boolean value to a record tag."
    | Value.String _ -> failwith "tried to assign string value to a record tag."
    | Value.Int _ -> failwith "tried to assign int value to a record tag."
    | Value.Float _ -> failwith "tried to assign float value to a record tag."
    | Value.Array _ -> failwith "tried to assign array value to a record tag."
    | Value.TemplateNode _ -> failwith "tried to assign template node to a record tag."
    | Value.DefinitionInfo _ ->
      failwith "tried to assign definition info to a record tag."
    | Value.Null -> output |> Output.add_value Value.Null
    | Value.Record r ->
      let models key = StringMap.find_opt key r in
      let state = State.make ~models state.State.declarations in
      let eval_property key (nullable, tag) =
        tag
        |> eval_tag ~output ~state:{ state with binding_identifier = Some key }
        |> Output.get_value
        |> function
        | Value.Null when not nullable ->
          failwith
            (Printf.sprintf
               "property `%s` on record tag is not marked as nullable, but was given a \
                null value."
               key)
        | value -> value
      in
      let r = of' |> StringMap.mapi eval_property in
      output |> Output.add_value (Value.Record r |> apply_transformer ~transformer:body))
  | Ast.TagSlot (attributes, body) ->
    let slot_name =
      attributes
      |> StringMap.find_opt "name"
      |> Option.map (eval_expression ~output ~state)
      |> Option.map Output.get_value
      |> Option.value ~default:(Value.String "")
      |> function
      | Value.String s -> s
      | _ -> failwith "Expected attribute `name` on #Slot to be of type string."
    in
    let min =
      attributes
      |> StringMap.find_opt "min"
      |> Option.map (eval_expression ~output ~state)
      |> Option.map Output.get_value
      |> Option.value ~default:(Value.Int 0)
      |> function
      | Value.Int i -> i
      | _ -> failwith "Expected attribute `min` on #Slot to be of type int."
    in
    let max =
      attributes
      |> StringMap.find_opt "max"
      |> Option.map (eval_expression ~output ~state)
      |> Option.map Output.get_value
      |> function
      | None -> None
      | Some (Value.Int i) -> Some i
      | _ -> failwith "Expected attribute `max` on #Slot to be of type int."
    in
    let instanceOf =
      attributes
      |> StringMap.find_opt "instanceOf"
      |> Option.map (eval_expression ~output ~state)
      |> Option.map Output.get_value
      |> function
      | None -> None
      | Some (Value.Array l) ->
        Some
          (l
          |> Iter.map (function
                 | Value.DefinitionInfo info -> info
                 | _ ->
                   failwith
                     "Expected attribute `instanceOf` on #Slot to be an array of \
                      uppercase identifiers."))
      | _ -> failwith "Expected attribute `instanceOf` on #Slot to be an array."
    in
    let output =
      output |> Output.add_tag ~typ:(get_output_typ tag) ("__slot:" ^ slot_name)
    in
    (match state.slotted_children with
    | None -> output |> Output.add_value Value.Null
    | Some children ->
      let find_slot_key attributes =
        attributes
        |> StringMap.find_opt "slot"
        |> Option.value ~default:(Value.String "")
        |> function
        | Value.String s -> s
        | _ -> failwith "Expected slot attribute to be of type string"
      in
      let check_instance_restriction tag f =
        match instanceOf with
        | None -> f
        | Some restrictions ->
          let is_in_list = ref false in
          let allowed, disallowed =
            restrictions
            |> Iter.to_rev_list
            |> List.partition_map (fun Value.{ name; negated; _ } ->
                   if name = tag then is_in_list := true;
                   if negated then Either.right name else Either.left name)
          in
          let is_in_list = !is_in_list in
          let is_allowed =
            match allowed, disallowed with
            | [], _disallowed -> not is_in_list
            | _allowed, [] -> is_in_list
            | allowed, _disallowed -> List.mem tag allowed
          in
          if not is_allowed
          then
            failwith
              (Printf.sprintf
                 "Child with tag `%s` may not be used inside the %s. The following \
                  restrictions are set: [ %s ]"
                 tag
                 (if slot_name = ""
                 then "Default #Slot."
                 else Printf.sprintf "#Slot with name `%s`" slot_name)
                 (instanceOf
                 |> Option.value ~default:Iter.empty
                 |> Iter.map (fun res ->
                        (if res.Value.negated then "!" else "") ^ res.Value.name)
                 |> Iter.intersperse ","
                 |> Iter.concat_str))
          else f
      in
      let rec keep_slotted acc = function
        | ( Value.TemplateNode (`Html, tag, attributes, _children, _self_closing)
          | Value.TemplateNode (`Component _, tag, attributes, _children, _self_closing)
            ) as value ->
          if find_slot_key attributes = slot_name
          then
            check_instance_restriction tag
            @@
            let transformed_value = value |> apply_transformer ~transformer:body in
            Iter.append acc (Iter.singleton transformed_value)
          else acc
        | Value.Array l -> l |> Iter.fold keep_slotted acc
        (* TODO: Decide on wether to render text nodes inside slots or not...
         | value when slot_name = "" -> Iter.append acc (Iter.singleton value) *)
        | _ -> acc
      in
      let slotted_children =
        children |> Iter.of_list |> Iter.fold keep_slotted Iter.empty
      in
      let amount_of_children = Iter.length slotted_children in
      (match slot_name, min, amount_of_children, max with
      | "", min, len, _ when len < min ->
        failwith
          (Printf.sprintf
             "Default #Slot did not reach the minimum amount of nodes (specified as %i)."
             min)
      | slot_name, min, len, _ when len < min ->
        failwith
          (Printf.sprintf
             "#Slot with name `%s` did not reach the minimum amount of nodes (specified \
              as %i)."
             slot_name
             min)
      | "", _, len, Some max when len > max ->
        failwith
          (Printf.sprintf
             "Default #Slot includes more than the maximum amount of nodes (specified as \
              %i)."
             max)
      | slot_name, _, len, Some max when len > max ->
        failwith
          (Printf.sprintf
             "#Slot with name `%s` includes more than the maximum amount of nodes \
              (specified as %i)."
             slot_name
             max)
      | _ -> output |> Output.add_value (Value.Array slotted_children)))

and eval_template ~output ~state template =
  match template with
  | Ast.TextTemplateNode text -> output |> Output.add_value (Value.String text)
  | Ast.HtmlTemplateNode { tag; attributes; children; self_closing } ->
    let attributes =
      attributes
      |> StringMap.map (eval_expression ~output ~state)
      |> StringMap.map Output.get_value
    in
    let children =
      children |> List.map (eval_template ~output ~state) |> List.map Output.get_value
    in
    output
    |> Output.add_value
         (Value.TemplateNode (`Html, tag, attributes, children, self_closing))
  | Ast.ExpressionTemplateNode expr -> eval_expression ~output ~state expr
  | Ast.ComponentTemplateNode { identifier = Uppercase_Id tag; attributes; children } ->
    let attributes =
      attributes
      |> StringMap.map (eval_expression ~output ~state)
      |> StringMap.map Output.get_value
    in
    let children =
      children |> List.map (eval_template ~output ~state) |> List.map Output.get_value
    in
    let render_fn ~models ~slotted_children =
      eval ~models ~slotted_children ~root:tag state.declarations |> Output.get_value
    in
    output
    |> Output.add_value
         (Value.TemplateNode (`Component render_fn, tag, attributes, children, false))

and eval_declaration ~output ~state declaration =
  match declaration with
  | Ast.ComponentDeclaration (_attrs, body)
  | Ast.SiteDeclaration (_attrs, body)
  | Ast.PageDeclaration (_attrs, body)
  | Ast.StoreDeclaration (_attrs, body) -> eval_expression ~output ~state body

and eval ?models ?slotted_children ~root declarations =
  let output = Output.empty in
  let state = State.make ?models ?slotted_children declarations in
  declarations
  |> StringMap.find_opt root
  |> function
  | Some declaration -> eval_declaration ~output ~state declaration
  | None -> failwith (Printf.sprintf "Declaration with name `%s` was not found." root)
;;

let file_contents chan = really_input_string chan (in_channel_length chan)

let from_directory ?models ?slotted_children ~directory root =
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
  eval ?models ?slotted_children ~root declarations
;;

let from_file ?models ?slotted_children ~filename root =
  let declarations = Parser.parse_file filename in
  eval ?models ?slotted_children ~root declarations
;;

let from_ast ?models ?slotted_children declarations root =
  eval ?models ?slotted_children ~root declarations
;;
