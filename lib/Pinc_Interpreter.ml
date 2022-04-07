module Parser = Pinc_Parser
module Ast = Pinc_Ast
module Value = Pinc_Interpreter_Generic.Value
module State = Pinc_Interpreter_Generic.State

exception Loop_Break
exception Loop_Continue

let should_never_happen () =
  let exception Internal_Error in
  raise Internal_Error
;;

let rec eval_statement ~state = function
  | Ast.LetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:false ~is_optional:false expression
  | Ast.OptionalLetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:false ~is_optional:true expression
  | Ast.OptionalMutableLetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:true ~is_optional:true expression
  | Ast.MutableLetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:true ~is_optional:false expression
  | Ast.MutationStatement (Lowercase_Id ident, expression) ->
    eval_mutation ~state ~ident expression
  | Ast.BreakStatement -> raise_notrace Loop_Break
  | Ast.ContinueStatement -> raise_notrace Loop_Continue
  | Ast.ExpressionStatement expression -> expression |> eval_expression ~state

and eval_expression ~state = function
  | Ast.Int i -> state |> State.add_output ~output:(`Int i)
  | Ast.Float f when Float.is_integer f ->
    state |> State.add_output ~output:(`Int (int_of_float f))
  | Ast.Float f -> state |> State.add_output ~output:(`Float f)
  | Ast.Bool b -> state |> State.add_output ~output:(`Bool b)
  | Ast.Array l ->
    state
    |> State.add_output
         ~output:
           (`Array
             (l |> Array.map (fun it -> it |> eval_expression ~state |> State.get_output)))
  | Ast.Record map ->
    state
    |> State.add_output
         ~output:
           (`Record
             (map
             |> StringMap.mapi (fun ident (optional, expression) ->
                    expression
                    |> eval_expression
                         ~state:
                           { state with
                             State.binding_identifier = Some (optional, ident)
                           }
                    |> State.get_output
                    |> function
                    | `Null when not optional ->
                      failwith
                        ("identifier "
                        ^ ident
                        ^ " is not marked as nullable, but was given a null value.")
                    | value -> value)))
  | Ast.String template -> eval_string_template ~state template
  | Ast.Function (parameters, body) -> eval_function_declaration ~state ~parameters body
  | Ast.FunctionCall (left, arguments) -> eval_function_call ~state ~arguments left
  | Ast.UppercaseIdentifierExpression (Uppercase_Id id) ->
    let value = state.State.declarations |> StringMap.find_opt id in
    let exists =
      match value with
      | None -> `DoesntExist
      | Some _ -> `Exists
    in
    state |> State.add_output ~output:(`DefinitionInfo (id, exists, `NotNegated))
  | Ast.LowercaseIdentifierExpression (Lowercase_Id id) ->
    eval_lowercase_identifier ~state id
  | Ast.TagExpression tag -> eval_tag ~state tag
  | Ast.ForInExpression { index; iterator = Lowercase_Id ident; reverse; iterable; body }
    -> eval_for_in ~state ~index_ident:index ~ident ~reverse ~iterable body
  | Ast.TemplateExpression nodes ->
    state
    |> State.add_output
         ~output:
           (`Array
             (nodes
             |> List.map (fun it -> it |> eval_template ~state |> State.get_output)
             |> Array.of_list))
  | Ast.BlockExpression e -> eval_block ~state e
  | Ast.ConditionalExpression { condition; consequent; alternate } ->
    eval_if ~state ~condition ~alternate ~consequent
  | Ast.UnaryExpression (Ast.Operators.Unary.NOT, expression) ->
    eval_unary_not ~state expression
  | Ast.UnaryExpression (Ast.Operators.Unary.MINUS, expression) ->
    eval_unary_minus ~state expression
  | Ast.BinaryExpression (left, Ast.Operators.Binary.EQUAL, right) ->
    eval_binary_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.NOT_EQUAL, right) ->
    eval_binary_not_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.GREATER, right) ->
    eval_binary_greater ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.GREATER_EQUAL, right) ->
    eval_binary_greater_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.LESS, right) ->
    eval_binary_less ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.LESS_EQUAL, right) ->
    eval_binary_less_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.PLUS, right) ->
    eval_binary_plus ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MINUS, right) ->
    eval_binary_minus ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.TIMES, right) ->
    eval_binary_times ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.DIV, right) ->
    eval_binary_div ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.POW, right) ->
    eval_binary_pow ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MODULO, right) ->
    eval_binary_modulo ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.CONCAT, right) ->
    eval_binary_concat ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.AND, right) ->
    eval_binary_and ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.OR, right) ->
    eval_binary_or ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.DOT_ACCESS, right) ->
    eval_binary_dot_access ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.BRACKET_ACCESS, right) ->
    eval_binary_bracket_access ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.ARRAY_ADD, right) ->
    eval_binary_array_add ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MERGE, right) ->
    eval_binary_merge ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.RANGE, right) ->
    eval_range ~state ~inclusive:false left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.INCLUSIVE_RANGE, right) ->
    eval_range ~state ~inclusive:true left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.FUNCTION_CALL, right) ->
    eval_function_call ~state ~arguments:[ right ] left
  | Ast.BinaryExpression (left, Ast.Operators.Binary.PIPE, right) ->
    eval_binary_pipe ~state left right

and eval_string_template ~state template =
  state
  |> State.add_output
       ~output:
         (`String
           (template
           |> List.map (function
                  | Ast.StringText s -> s
                  | Ast.StringInterpolation e ->
                    eval_expression ~state e |> State.get_output |> Value.to_string)
           |> String.concat ""))

and eval_function_declaration ~state ~parameters body =
  let ident = state.State.binding_identifier in
  let self = ref `Null in
  let exec ~arguments ~state () =
    let state =
      match ident with
      | None -> state
      | Some (_, ident) ->
        state
        |> State.add_value_to_scope
             ~ident
             ~value:!self
             ~is_mutable:false
             ~is_optional:false
    in
    let state =
      state
      |> State.add_scope
      |> StringMap.fold
           (fun ident value ->
             State.add_value_to_scope ~ident ~value ~is_mutable:false ~is_optional:false)
           arguments
    in
    eval_expression ~state body |> State.get_output
  in
  let fn = `Function Value.{ parameters; state; exec } in
  ident
  |> Option.iter (fun (_, ident) ->
         state
         |> State.add_value_to_function_scopes
              ~ident
              ~value:fn
              ~is_optional:false
              ~is_mutable:false);
  self := fn;
  state |> State.add_output ~output:fn

and eval_function_call ~state ~arguments left =
  let maybe_fn = eval_expression ~state left |> State.get_output in
  match maybe_fn with
  | `Function { parameters; state = fn_state; exec }
    when List.compare_lengths parameters arguments = 0 ->
    let arguments =
      List.combine parameters arguments
      |> List.fold_left
           (fun acc (param, arg) ->
             let value = arg |> eval_expression ~state |> State.get_output in
             acc |> StringMap.add param value)
           StringMap.empty
    in
    state |> State.add_output ~output:(exec ~arguments ~state:fn_state ())
  | `Function { parameters; state = _; exec = _ } ->
    if List.compare_lengths parameters arguments > 0
    then (
      let arguments_len = List.length arguments in
      let missing =
        parameters
        |> List.filteri (fun index _ -> index > arguments_len - 1)
        |> List.map (fun item -> "`" ^ item ^ "`")
        |> String.concat ", "
      in
      failwith
        ("This function was provided too few arguments. The following parameters are \
          missing: "
        ^ missing))
    else
      failwith
        ("This function only accepts "
        ^ string_of_int (List.length parameters)
        ^ " arguments, but was provided "
        ^ string_of_int (List.length arguments)
        ^ " here.")
  | _ -> failwith "Trying to call a non function value"

and eval_binary_pipe ~state left right =
  let right =
    match right with
    | Ast.FunctionCall (fn, arguments) -> Ast.FunctionCall (fn, left :: arguments)
    | fn -> Ast.FunctionCall (fn, [ left ])
  in
  right |> eval_expression ~state

and eval_binary_plus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `Int a, `Int b -> state |> State.add_output ~output:(`Int (a + b))
  | `Float a, `Float b -> state |> State.add_output ~output:(`Float (a +. b))
  | `Float a, `Int b -> state |> State.add_output ~output:(`Float (a +. float_of_int b))
  | `Int a, `Float b -> state |> State.add_output ~output:(`Float (float_of_int a +. b))
  | _ -> failwith "Trying to add non numeric literals."

and eval_binary_minus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `Int a, `Int b -> state |> State.add_output ~output:(`Int (a - b))
  | `Float a, `Float b -> state |> State.add_output ~output:(`Float (a -. b))
  | `Float a, `Int b -> state |> State.add_output ~output:(`Float (a -. float_of_int b))
  | `Int a, `Float b -> state |> State.add_output ~output:(`Float (float_of_int a -. b))
  | _ -> failwith "Trying to subtract non numeric literals."

and eval_binary_times ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `Int a, `Int b -> state |> State.add_output ~output:(`Int (a * b))
  | `Float a, `Float b -> state |> State.add_output ~output:(`Float (a *. b))
  | `Float a, `Int b -> state |> State.add_output ~output:(`Float (a *. float_of_int b))
  | `Int a, `Float b -> state |> State.add_output ~output:(`Float (float_of_int a *. b))
  | _ -> failwith "Trying to multiply non numeric literals."

and eval_binary_div ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let r =
    match a, b with
    | `Int _, `Int 0 -> failwith "Trying to divide by 0"
    | `Float _, `Float 0. -> failwith "Trying to divide by 0"
    | `Float _, `Int 0 -> failwith "Trying to divide by 0"
    | `Int _, `Float 0. -> failwith "Trying to divide by 0"
    | `Int a, `Int b -> float_of_int a /. float_of_int b
    | `Float a, `Float b -> a /. b
    | `Float a, `Int b -> a /. float_of_int b
    | `Int a, `Float b -> float_of_int a /. b
    | _ -> failwith "Trying to divide non numeric literals."
  in
  if Float.is_integer r
  then state |> State.add_output ~output:(`Int (int_of_float r))
  else state |> State.add_output ~output:(`Float r)

and eval_binary_pow ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let r =
    match a, b with
    | `Int a, `Int b -> float_of_int a ** float_of_int b
    | `Float a, `Float b -> a ** b
    | `Float a, `Int b -> a ** float_of_int b
    | `Int a, `Float b -> float_of_int a ** b
    | _ -> failwith "Trying to raise non numeric literals."
  in
  if Float.is_integer r
  then state |> State.add_output ~output:(`Int (int_of_float r))
  else state |> State.add_output ~output:(`Float r)

and eval_binary_modulo ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let ( %. ) = mod_float in
  let ( % ) = ( mod ) in
  let r =
    match a, b with
    | `Int _, `Int 0 -> failwith "Trying to modulo with 0 on right hand side."
    | `Int _, `Float 0. -> failwith "Trying to modulo with 0 on right hand side."
    | `Float _, `Float 0. -> failwith "Trying to modulo with 0 on right hand side."
    | `Float _, `Int 0 -> failwith "Trying to modulo with 0 on right hand side."
    | `Int a, `Int b -> a % b
    | `Float a, `Float b -> int_of_float (a %. b)
    | `Float a, `Int b -> int_of_float a % b
    | `Int a, `Float b -> a % int_of_float b
    | _ -> failwith "Trying to modulo non numeric literals."
  in
  state |> State.add_output ~output:(`Int r)

and eval_binary_and ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.is_true a && Value.is_true b))

and eval_binary_or ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.is_true a || Value.is_true b))

and eval_binary_less ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b < 0))

and eval_binary_less_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b <= 0))

and eval_binary_greater ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b > 0))

and eval_binary_greater_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b >= 0))

and eval_binary_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.equal a b))

and eval_binary_not_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (not (Value.equal a b)))

and eval_binary_concat ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `String a, `String b -> state |> State.add_output ~output:(`String (a ^ b))
  | _ -> failwith "Trying to concat non string literals."

and eval_binary_dot_access ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Record left, Ast.LowercaseIdentifierExpression (Lowercase_Id b) ->
    let output = left |> StringMap.find_opt b |> Option.value ~default:`Null in
    state |> State.add_output ~output
  | ( `TemplateNode (_typ, tag, attributes, children, _self_closing)
    , Ast.LowercaseIdentifierExpression (Lowercase_Id b) ) ->
    (match b with
    | "tag" -> state |> State.add_output ~output:(`String tag)
    | "attributes" -> state |> State.add_output ~output:(`Record attributes)
    | "children" -> state |> State.add_output ~output:(`Array (Array.of_list children))
    | s ->
      failwith
        ("Unknown property "
        ^ s
        ^ " on template node. Known properties are: `tag`, `attributes` and `children`."))
  | `Record _, _ ->
    failwith "Expected right hand side of record access to be a lowercase identifier."
  | `Null, _ -> state |> State.add_output ~output:`Null
  | _, Ast.LowercaseIdentifierExpression _ ->
    failwith "Trying to access a property on a non record or template value."
  | _ -> failwith "I am really not sure what you are trying to do here..."

and eval_binary_bracket_access ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Array left, `Int right ->
    let output =
      try left.(right) with
      | Invalid_argument _ -> `Null
    in
    state |> State.add_output ~output
  | `Record left, `String right ->
    let output = left |> StringMap.find_opt right |> Option.value ~default:`Null in
    state |> State.add_output ~output
  | `Null, _ -> state |> State.add_output ~output:`Null
  | `Array _, _ -> failwith "Cannot access array with a non integer value."
  | `Record _, _ -> failwith "Cannot access record with a non string value."
  | _ -> failwith "Trying to access a property on a non record or array value."

and eval_binary_array_add ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Array left, value ->
    state |> State.add_output ~output:(`Array (Array.append left [| value |]))
  | _ -> failwith "Trying to add an element on a non array value."

and eval_binary_merge ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Array left, `Array right ->
    state |> State.add_output ~output:(`Array (Array.append left right))
  | `Record left, `Record right ->
    state
    |> State.add_output
         ~output:(`Record (StringMap.union (fun _key _x y -> Some y) left right))
  | `TemplateNode (typ, tag, attributes, children, self_closing), `Record right ->
    let attributes = StringMap.union (fun _key _x y -> Some y) attributes right in
    state
    |> State.add_output
         ~output:(`TemplateNode (typ, tag, attributes, children, self_closing))
  | `TemplateNode _, _ ->
    failwith "Trying to merge a non record value onto tag attributes."
  | `Array _, _ -> failwith "Trying to merge a non array value onto an array."
  | _, `Array _ -> failwith "Trying to merge an array value onto a non array."
  | _ -> failwith "Trying to merge two non array values."

and eval_unary_not ~state expression =
  match eval_expression ~state expression |> State.get_output with
  | `DefinitionInfo (name, exists, negated) ->
    let negated =
      match negated with
      | `Negated -> `NotNegated
      | `NotNegated -> `Negated
    in
    state |> State.add_output ~output:(`DefinitionInfo (name, exists, negated))
  | v -> state |> State.add_output ~output:(`Bool (not (Value.is_true v)))

and eval_unary_minus ~state expression =
  match eval_expression ~state expression |> State.get_output with
  | `Int i -> state |> State.add_output ~output:(`Int (Int.neg i))
  | `Float f -> state |> State.add_output ~output:(`Float (Float.neg f))
  | _ ->
    failwith
      "Invalid usage of unary `-` operator. You are only able to negate integers or \
       floats."

and eval_lowercase_identifier ~state ident =
  state
  |> State.get_value_from_scope ~ident
  |> function
  | None -> failwith ("Unbound identifier `" ^ ident ^ "`")
  | Some { value; is_mutable = _; is_optional = _ } ->
    state |> State.add_output ~output:value

and eval_let ~state ~ident ~is_mutable ~is_optional expression =
  let state =
    expression
    |> eval_expression
         ~state:{ state with State.binding_identifier = Some (is_optional, ident) }
  in
  match State.get_output state with
  | `Null when not is_optional ->
    failwith
      ("identifier " ^ ident ^ " is not marked as nullable, but was given a null value.")
  | value ->
    state
    |> State.add_value_to_scope ~ident ~value ~is_mutable ~is_optional
    |> State.add_output ~output:`Null

and eval_mutation ~state ~ident expression =
  let current_binding = State.get_value_from_scope ~ident state in
  match current_binding with
  | None ->
    failwith "Trying to update a variable, which does not exist in the current scope."
  | Some { is_mutable = false; _ } -> failwith "Trying to update a non mutable variable."
  | Some { value = _; is_mutable = true; is_optional } ->
    let output =
      expression
      |> eval_expression
           ~state:{ state with State.binding_identifier = Some (is_optional, ident) }
    in
    let () =
      output
      |> State.get_output
      |> function
      | `Null when not is_optional ->
        failwith
          ("identifier "
          ^ ident
          ^ " is not marked as nullable, but was tried to be updated with a null value.")
      | value -> state |> State.update_value_in_scope ~ident ~value
    in
    state |> State.add_output ~output:`Null

and eval_if ~state ~condition ~alternate ~consequent =
  let condition_matches =
    condition |> eval_expression ~state |> State.get_output |> Value.is_true
  in
  match condition_matches, alternate with
  | true, _ -> consequent |> eval_statement ~state
  | false, Some alt -> alt |> eval_statement ~state
  | false, None -> state |> State.add_output ~output:`Null

and eval_for_in ~state ~index_ident ~ident ~reverse ~iterable body =
  let make_rev array =
    array |> Array.stable_sort (fun _ _ -> 1);
    array
  in
  let iterable = iterable |> eval_expression ~state |> State.get_output in
  let maybe_rev = if reverse then make_rev else fun arr -> arr in
  let index = ref (-1) in
  let rec loop acc = function
    | [] -> List.rev acc
    | value :: tl ->
      index := succ !index;
      let state =
        state
        |> State.add_value_to_scope ~ident ~value ~is_mutable:false ~is_optional:false
      in
      let state =
        match index_ident with
        | Some (Lowercase_Id ident) ->
          state
          |> State.add_value_to_scope
               ~ident
               ~value:(`Int !index)
               ~is_mutable:false
               ~is_optional:false
        | None -> state
      in
      (match eval_statement ~state body with
      | exception Loop_Continue -> loop acc tl
      | exception Loop_Break -> List.rev acc
      | v -> loop (State.get_output v :: acc) tl)
  in
  match iterable with
  | `Array l ->
    let res = l |> maybe_rev |> Array.to_list |> loop [] |> Array.of_list in
    state |> State.add_output ~output:(`Array res)
  | `String s ->
    let res =
      s
      |> String.to_seq
      |> Array.of_seq
      |> maybe_rev
      |> Array.map (fun c -> `String (String.make 1 c))
      |> Array.to_list
      |> loop []
      |> Array.of_list
    in
    state |> State.add_output ~output:(`Array res)
  | `Null -> state |> State.add_output ~output:`Null
  | `TemplateNode _ -> failwith "Cannot iterate over template node"
  | `Record _ -> failwith "Cannot iterate over record value"
  | `Int _ -> failwith "Cannot iterate over int value"
  | `Float _ -> failwith "Cannot iterate over float value"
  | `Bool _ -> failwith "Cannot iterate over boolean value"
  | `DefinitionInfo _ -> failwith "Cannot iterate over definition info"
  | `Function _ -> failwith "Cannot iterate over function"
  | `TagInfo _ -> should_never_happen ()

and eval_range ~state ~inclusive from upto =
  let from = from |> eval_expression ~state |> State.get_output in
  let upto = upto |> eval_expression ~state |> State.get_output in
  let from, upto =
    match from, upto with
    | `Int from, `Int upto -> from, upto
    | `Int _, _ ->
      failwith
        "Can't construct range in for loop. The end of your range is not of type int."
    | _, `Int _ ->
      failwith
        "Can't construct range in for loop. The start of your range is not of type int."
    | _, _ ->
      failwith
        "Can't construct range in for loop. The start and end of your range are not of \
         type int."
  in
  let iter =
    if from > upto
    then [||]
    else (
      let start = from in
      let stop = if not inclusive then upto else upto + 1 in
      Array.init (stop - start) (fun i -> `Int (i + start)))
  in
  state |> State.add_output ~output:(`Array iter)

and eval_block ~state statements =
  let state = state |> State.add_scope in
  statements |> List.fold_left (fun state -> eval_statement ~state) state

and eval_tag ~state tag =
  let Ast.{ tag_name; attributes; transformer } = tag in
  let is_optional =
    state.binding_identifier |> Option.map fst |> Option.value ~default:false
  in
  let attributes =
    attributes
    |> StringMap.map (fun it ->
           it |> eval_expression ~state:{ state with tag_info = true } |> State.get_output)
  in
  let apply_transformer ~transformer value =
    match transformer with
    | Some (Ast.Lowercase_Id ident, expr) ->
      let state =
        state
        |> State.add_scope
        |> State.add_value_to_scope ~ident ~value ~is_optional:false ~is_mutable:false
      in
      eval_expression ~state expr |> State.get_output
    | _ -> value
  in
  let attributes =
    match StringMap.find_opt "key" attributes, state.binding_identifier with
    | None, Some ident -> attributes |> StringMap.add "key" (`String (snd ident))
    | Some (`String _), _ -> attributes
    | Some _, _ -> failwith "Expected attribute `key` on tag to be of type string"
    | None, None -> attributes
  in
  let tag = tag_name, is_optional, attributes, apply_transformer ~transformer in
  let value =
    match state.tag_info with
    | false -> state |> State.call_tag_listener ~key:("#" ^ tag_name) ~tag
    | true -> `TagInfo tag
  in
  state |> State.add_output ~output:value

and eval_template ~state template =
  match template with
  | Ast.TextTemplateNode text -> state |> State.add_output ~output:(`String text)
  | Ast.HtmlTemplateNode { tag; attributes; children; self_closing } ->
    let attributes =
      attributes
      |> StringMap.map (eval_expression ~state)
      |> StringMap.map State.get_output
    in
    let children =
      children |> List.map (eval_template ~state) |> List.map State.get_output
    in
    state
    |> State.add_output
         ~output:(`TemplateNode (`Html, tag, attributes, children, self_closing))
  | Ast.ExpressionTemplateNode expr -> eval_expression ~state expr
  | Ast.ComponentTemplateNode { identifier = Uppercase_Id tag; attributes; children } ->
    let attributes =
      attributes
      |> StringMap.map (eval_expression ~state)
      |> StringMap.map State.get_output
    in
    let children =
      children |> List.map (eval_template ~state) |> List.map State.get_output
    in
    let component_tag_listeters =
      StringMap.empty
      |> StringMap.add "#String" (Pinc_Tags.make_string attributes)
      |> StringMap.add "#Int" (Pinc_Tags.make_int attributes)
      |> StringMap.add "#Float" (Pinc_Tags.make_float attributes)
      |> StringMap.add "#Boolean" (Pinc_Tags.make_boolean attributes)
      |> StringMap.add "#Array" (Pinc_Tags.make_array attributes)
      |> StringMap.add "#Record" (Pinc_Tags.make_record attributes)
      |> StringMap.add "#Slot" (Pinc_Tags.make_slot children)
    in
    let tag_listeners =
      component_tag_listeters
      |> StringMap.union (fun _key _x y -> Some y) state.tag_listeners
    in
    let render_fn () =
      let state = State.make ~tag_listeners state.declarations in
      state.declarations
      |> StringMap.find_opt tag
      |> (function
           | Some declaration -> eval_declaration ~state declaration
           | None -> failwith ("Declaration with name `" ^ tag ^ "` was not found."))
      |> State.get_output
    in
    state
    |> State.add_output
         ~output:(`TemplateNode (`Component render_fn, tag, attributes, children, false))

and eval_declaration ~state declaration =
  match declaration with
  | Ast.ComponentDeclaration (_attrs, body)
  | Ast.SiteDeclaration (_attrs, body)
  | Ast.PageDeclaration (_attrs, body)
  | Ast.StoreDeclaration (_attrs, body) -> eval_expression ~state body
;;

let eval ?tag_listeners ~root declarations =
  let context = Hashtbl.create 10 in
  let tag_listeners =
    let default_tag_listeters =
      StringMap.empty
      |> StringMap.add "#SetContext" (Pinc_Tags.make_set_context context)
      |> StringMap.add "#GetContext" (Pinc_Tags.make_get_context context)
    in
    match tag_listeners with
    | None -> default_tag_listeters
    | Some listeners ->
      default_tag_listeters |> StringMap.union (fun _key x _y -> Some x) listeners
  in
  let state = State.make ~tag_listeners declarations in
  declarations
  |> StringMap.find_opt root
  |> function
  | Some declaration -> eval_declaration ~state declaration
  | None -> failwith ("Declaration with name `" ^ root ^ "` was not found.")
;;

let from_source ?(filename = "") ~source root =
  let declarations = Parser.parse ~filename source in
  eval ~root declarations
;;
