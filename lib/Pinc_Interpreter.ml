module Parser = Pinc_Parser
module Ast = Pinc_Ast
open Pinc_Interpreter_Types

exception Loop_Break

exception Loop_Continue

let should_never_happen () =
  let exception Internal_Error in
  raise Internal_Error

module Value = struct
  let null () = Null

  let of_string s = String s

  let of_bool b = Bool b

  let of_int i = Int i

  let of_float f = Float f

  let of_array l = Array l

  let of_list l = Array (Array.of_list l)

  let of_string_map m = Record m

  let make_component ~render ~tag ~attributes =
    ComponentTemplateNode (render, tag, attributes)

  let rec to_string = function
    | Null ->
        ""
    | String s ->
        s
    | Int i ->
        string_of_int i
    | Float f when Float.is_integer f ->
        string_of_int (int_of_float f)
    | Float f ->
        string_of_float f
    | Bool b ->
        if b then "true" else "false"
    | Array l ->
        let buf = Buffer.create 200 in
        l
        |> Array.iteri (fun i it ->
               if i <> 0 then Buffer.add_char buf '\n' ;
               Buffer.add_string buf (to_string it) ) ;
        Buffer.contents buf
    | Record m ->
        let b = Buffer.create 1024 in
        m
        |> StringMap.iter (fun _key value ->
               Buffer.add_string b (to_string value) ;
               Buffer.add_char b '\n' ) ;
        Buffer.contents b
    | HtmlTemplateNode (tag, attributes, children, self_closing) ->
        let buf = Buffer.create 128 in
        Buffer.add_char buf '<' ;
        Buffer.add_string buf tag ;
        if not (StringMap.is_empty attributes) then
          attributes
          |> StringMap.iter (fun key value ->
                 match value with
                 | Null ->
                     ()
                 | Function _
                 | String _
                 | Int _
                 | Float _
                 | Bool _
                 | Array _
                 | Record _
                 | HtmlTemplateNode _
                 | ComponentTemplateNode _
                 | DefinitionInfo _ ->
                     Buffer.add_char buf ' ' ;
                     Buffer.add_string buf key ;
                     Buffer.add_char buf '=' ;
                     Buffer.add_char buf '"' ;
                     Buffer.add_string buf (value |> to_string) ;
                     Buffer.add_char buf '"'
                 | TagInfo _ ->
                     assert false ) ;
        if self_closing && Pinc_HTML.is_void_el tag then
          Buffer.add_string buf " />"
        else (
          Buffer.add_char buf '>' ;
          children
          |> List.iter (fun child -> Buffer.add_string buf (to_string child)) ;
          Buffer.add_char buf '<' ;
          Buffer.add_char buf '/' ;
          Buffer.add_string buf tag ;
          Buffer.add_char buf '>' ) ;
        Buffer.contents buf
    | ComponentTemplateNode (render_fn, _tag, attributes) ->
        attributes |> render_fn |> to_string
    | Function _ ->
        ""
    | DefinitionInfo _ ->
        ""
    | TagInfo _ ->
        assert false

  let is_true = function
    | Null ->
        false
    | Bool b ->
        b
    | String s ->
        s |> String.trim |> String.length > 0
    | Int _ ->
        true
    | Float _ ->
        true
    | HtmlTemplateNode _ ->
        true
    | ComponentTemplateNode _ ->
        true
    | DefinitionInfo (_name, Some _, _negated) ->
        true
    | DefinitionInfo (_name, None, _negated) ->
        false
    | Function _ ->
        true
    | Array [||] ->
        false
    | Array _ ->
        true
    | Record m ->
        not (StringMap.is_empty m)
    | TagInfo _ ->
        assert false

  let rec equal a b =
    match (a, b) with
    | String a, String b ->
        String.equal a b
    | Int a, Int b ->
        a = b
    | Float a, Float b ->
        a = b
    | Float a, Int b ->
        a = float_of_int b
    | Int a, Float b ->
        float_of_int a = b
    | Bool a, Bool b ->
        a = b
    | Array a, Array b ->
        a = b
    | Record a, Record b ->
        StringMap.equal equal a b
    | Function _, Function _ ->
        false
    | DefinitionInfo (a, _, _), DefinitionInfo (b, _, _) ->
        String.equal a b
    | ( HtmlTemplateNode (a_tag, a_attrs, a_children, a_self_closing)
      , HtmlTemplateNode (b_tag, b_attrs, b_children, b_self_closing) ) ->
        a_tag = b_tag
        && a_self_closing = b_self_closing
        && StringMap.equal equal a_attrs b_attrs
        && a_children = b_children
    | ( ComponentTemplateNode (_, a_tag, a_attributes)
      , ComponentTemplateNode (_, b_tag, b_attributes) ) ->
        a_tag = b_tag && StringMap.equal equal a_attributes b_attributes
    | Null, Null ->
        true
    | TagInfo _, _ ->
        assert false
    | _, TagInfo _ ->
        assert false
    | _ ->
        false

  let rec compare a b =
    match (a, b) with
    | String a, String b ->
        String.compare a b
    | Int a, Int b ->
        Int.compare a b
    | Float a, Float b ->
        Float.compare a b
    | Float a, Int b ->
        Float.compare a (float_of_int b)
    | Int a, Float b ->
        Float.compare (float_of_int a) b
    | Bool a, Bool b ->
        Bool.compare a b
    | Array a, Array b ->
        Int.compare (Array.length a) (Array.length b)
    | Record a, Record b ->
        StringMap.compare compare a b
    | Null, Null ->
        0
    | ComponentTemplateNode _, ComponentTemplateNode _ ->
        0
    | HtmlTemplateNode _, HtmlTemplateNode _ ->
        0
    | DefinitionInfo _, DefinitionInfo _ ->
        0
    | Function _, Function _ ->
        0
    | TagInfo _, _ ->
        assert false
    | _, TagInfo _ ->
        assert false
    | _ ->
        assert false
end

module State = struct
  let make ?(tag_listeners = StringMap.empty) ?parent_component
      ?(context = Hashtbl.create 10) declarations =
    { binding_identifier= None
    ; declarations
    ; output= Null
    ; environment= {scope= []; use_scope= []}
    ; tag_listeners
    ; tag_info= false
    ; parent_component
    ; context }

  let add_scope t =
    let environment = {t.environment with scope= [] :: t.environment.scope} in
    {t with environment}

  let add_value_to_scope ~ident ~value ~is_optional ~is_mutable t =
    let update_scope t =
      match t.environment.scope with
      | [] ->
          assert false
      | scope :: rest ->
          ((ident, {is_mutable; is_optional; value}) :: scope) :: rest
    in
    let environment = {t.environment with scope= update_scope t} in
    {t with environment}

  let add_value_to_use_scope ~ident ~value t =
    let use_scope = (ident, value) :: t.environment.use_scope in
    let environment = {t.environment with use_scope} in
    {t with environment}

  let update_value_in_scope ~ident ~value t =
    let updated = ref false in
    let rec update_scope state =
      List.map
        (function
          | scope when not !updated ->
              (List.map (function
                | key, binding
                  when (not !updated) && key = ident && binding.is_mutable ->
                    updated := true ;
                    (key, {binding with value})
                | ( key
                  , ( {value= Function {state= fn_state; parameters; exec}; _}
                    as binding ) )
                  when not !updated ->
                    fn_state.environment.scope <- update_scope fn_state ;
                    ( key
                    , { binding with
                        value= Function {state= fn_state; parameters; exec} } )
                | v ->
                    v ) )
                scope
          | scope ->
              scope )
        state.environment.scope
    in
    t.environment.scope <- update_scope t

  let add_value_to_function_scopes ~ident ~value ~is_optional ~is_mutable t =
    let update_scope state =
      List.map
        (List.map (function
          | key, ({value= Function {state; parameters; exec}; _} as binding) ->
              let new_state =
                add_value_to_scope ~ident ~value ~is_optional ~is_mutable state
              in
              ( key
              , { binding with
                  value= Function {state= new_state; parameters; exec} } )
          | v ->
              v ) )
        state.environment.scope
    in
    t.environment.scope <- update_scope t

  let get_value_from_scope ~ident t =
    t.environment.scope |> List.find_map (List.assoc_opt ident)

  let get_output t = t.output

  let add_output ~output t = {t with output}

  let get_bindings t = t.environment.scope |> List.hd

  let get_used_values t = t.environment.use_scope

  let call_tag_listener ~key ~tag t =
    match t.tag_listeners |> StringMap.find_opt key with
    | None ->
        Null
    | Some listener -> (
        tag
        |> listener.eval ~self:listener t
        |> function Ok v -> v | Error e -> failwith e )

  let get_parent_component t = t.parent_component
end

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
  | Ast.UseStatement (Uppercase_Id ident, expression) ->
      eval_use ~state ~ident expression
  | Ast.BreakStatement ->
      raise_notrace Loop_Break
  | Ast.ContinueStatement ->
      raise_notrace Loop_Continue
  | Ast.ExpressionStatement expression ->
      expression |> eval_expression ~state

and eval_expression ~state = function
  | Ast.Int i ->
      state |> State.add_output ~output:(Value.of_int i)
  | Ast.Float f when Float.is_integer f ->
      state |> State.add_output ~output:(Value.of_int (int_of_float f))
  | Ast.Float f ->
      state |> State.add_output ~output:(Value.of_float f)
  | Ast.Bool b ->
      state |> State.add_output ~output:(Value.of_bool b)
  | Ast.Array l ->
      state
      |> State.add_output
           ~output:
             ( l
             |> Array.map (fun it ->
                    it |> eval_expression ~state |> State.get_output )
             |> Value.of_array )
  | Ast.Record map ->
      state
      |> State.add_output
           ~output:
             ( map
             |> StringMap.mapi (fun ident (optional, expression) ->
                    expression
                    |> eval_expression
                         ~state:
                           { state with
                             binding_identifier= Some (optional, ident) }
                    |> State.get_output
                    |> function
                    | Null when not optional ->
                        failwith
                          ( "identifier " ^ ident
                          ^ " is not marked as nullable, but was given a null \
                             value." )
                    | value ->
                        value )
             |> Value.of_string_map )
  | Ast.String template ->
      eval_string_template ~state template
  | Ast.Function (parameters, body) ->
      eval_function_declaration ~state ~parameters body
  | Ast.FunctionCall (left, arguments) ->
      eval_function_call ~state ~arguments left
  | Ast.UppercaseIdentifierExpression (Uppercase_Id id) ->
      let value = state.declarations |> StringMap.find_opt id in
      let typ =
        match value with
        | None ->
            None
        | Some (Ast.ComponentDeclaration _) ->
            Some `Component
        | Some (Ast.PageDeclaration _) ->
            Some `Page
        | Some (Ast.SiteDeclaration _) ->
            Some `Site
        | Some (Ast.StoreDeclaration _) ->
            Some `Store
        | Some (Ast.LibraryDeclaration _ as declaration) ->
            (*
               NOTE: Do we really want to evaluate the library with the current state?
                     Or do we maybe want to create a fresh state?
            *)
            let declaration = eval_declaration ~state declaration in
            let top_level_bindings = declaration |> State.get_bindings in
            let used_values = declaration |> State.get_used_values in
            Some (`Library (top_level_bindings, used_values))
      in
      state |> State.add_output ~output:(DefinitionInfo (id, typ, `NotNegated))
  | Ast.LowercaseIdentifierExpression (Lowercase_Id id) ->
      eval_lowercase_identifier ~state id
  | Ast.TagExpression tag ->
      eval_tag ~state tag
  | Ast.ForInExpression
      {index; iterator= Lowercase_Id ident; reverse; iterable; body} ->
      eval_for_in ~state ~index_ident:index ~ident ~reverse ~iterable body
  | Ast.TemplateExpression nodes ->
      state
      |> State.add_output
           ~output:
             ( nodes
             |> List.map (fun it ->
                    it |> eval_template ~state |> State.get_output )
             |> Value.of_list )
  | Ast.BlockExpression e ->
      eval_block ~state e
  | Ast.ConditionalExpression {condition; consequent; alternate} ->
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
      eval_function_call ~state ~arguments:[right] left
  | Ast.BinaryExpression (left, Ast.Operators.Binary.PIPE, right) ->
      eval_binary_pipe ~state left right

and eval_string_template ~state template =
  state
  |> State.add_output
       ~output:
         ( template
         |> List.map (function
              | Ast.StringText s ->
                  s
              | Ast.StringInterpolation e ->
                  eval_expression ~state e |> State.get_output
                  |> Value.to_string )
         |> String.concat "" |> Value.of_string )

and eval_function_declaration ~state ~parameters body =
  let ident = state.binding_identifier in
  let self = ref Null in
  let exec ~arguments ~state () =
    let state =
      match ident with
      | None ->
          state
      | Some (_, ident) ->
          state
          |> State.add_value_to_scope ~ident ~value:!self ~is_mutable:false
               ~is_optional:false
    in
    let state =
      state |> State.add_scope
      |> StringMap.fold
           (fun ident value ->
             State.add_value_to_scope ~ident ~value ~is_mutable:false
               ~is_optional:false )
           arguments
    in
    eval_expression ~state body |> State.get_output
  in
  let fn = Function {parameters; state; exec} in
  ident
  |> Option.iter (fun (_, ident) ->
         state
         |> State.add_value_to_function_scopes ~ident ~value:fn
              ~is_optional:false ~is_mutable:false ) ;
  self := fn ;
  state |> State.add_output ~output:fn

and eval_function_call ~state ~arguments left =
  let maybe_fn = eval_expression ~state left |> State.get_output in
  match maybe_fn with
  | Function {parameters; state= fn_state; exec}
    when List.compare_lengths parameters arguments = 0 ->
      let arguments =
        List.combine parameters arguments
        |> List.fold_left
             (fun acc (param, arg) ->
               let value = arg |> eval_expression ~state |> State.get_output in
               acc |> StringMap.add param value )
             StringMap.empty
      in
      state |> State.add_output ~output:(exec ~arguments ~state:fn_state ())
  | Function {parameters; state= _; exec= _} ->
      if List.compare_lengths parameters arguments > 0 then
        let arguments_len = List.length arguments in
        let missing =
          parameters
          |> List.filteri (fun index _ -> index > arguments_len - 1)
          |> List.map (fun item -> "`" ^ item ^ "`")
          |> String.concat ", "
        in
        failwith
          ( "This function was provided too few arguments. The following \
             parameters are missing: " ^ missing )
      else
        failwith
          ( "This function only accepts "
          ^ string_of_int (List.length parameters)
          ^ " arguments, but was provided "
          ^ string_of_int (List.length arguments)
          ^ " here." )
  | _ ->
      failwith "Trying to call a non function value"

and eval_binary_pipe ~state left right =
  let right =
    match right with
    | Ast.FunctionCall (fn, arguments) ->
        Ast.FunctionCall (fn, left :: arguments)
    | fn ->
        Ast.FunctionCall (fn, [left])
  in
  right |> eval_expression ~state

and eval_binary_plus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match (a, b) with
  | Int a, Int b ->
      state |> State.add_output ~output:(Int (a + b))
  | Float a, Float b ->
      state |> State.add_output ~output:(Float (a +. b))
  | Float a, Int b ->
      state |> State.add_output ~output:(Float (a +. float_of_int b))
  | Int a, Float b ->
      state |> State.add_output ~output:(Float (float_of_int a +. b))
  | _ ->
      failwith "Trying to add non numeric literals."

and eval_binary_minus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match (a, b) with
  | Int a, Int b ->
      state |> State.add_output ~output:(Int (a - b))
  | Float a, Float b ->
      state |> State.add_output ~output:(Float (a -. b))
  | Float a, Int b ->
      state |> State.add_output ~output:(Float (a -. float_of_int b))
  | Int a, Float b ->
      state |> State.add_output ~output:(Float (float_of_int a -. b))
  | _ ->
      failwith "Trying to subtract non numeric literals."

and eval_binary_times ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match (a, b) with
  | Int a, Int b ->
      state |> State.add_output ~output:(Int (a * b))
  | Float a, Float b ->
      state |> State.add_output ~output:(Float (a *. b))
  | Float a, Int b ->
      state |> State.add_output ~output:(Float (a *. float_of_int b))
  | Int a, Float b ->
      state |> State.add_output ~output:(Float (float_of_int a *. b))
  | _ ->
      failwith "Trying to multiply non numeric literals."

and eval_binary_div ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let r =
    match (a, b) with
    | Int _, Int 0 ->
        failwith "Trying to divide by 0"
    | Float _, Float 0. ->
        failwith "Trying to divide by 0"
    | Float _, Int 0 ->
        failwith "Trying to divide by 0"
    | Int _, Float 0. ->
        failwith "Trying to divide by 0"
    | Int a, Int b ->
        float_of_int a /. float_of_int b
    | Float a, Float b ->
        a /. b
    | Float a, Int b ->
        a /. float_of_int b
    | Int a, Float b ->
        float_of_int a /. b
    | _ ->
        failwith "Trying to divide non numeric literals."
  in
  if Float.is_integer r then
    state |> State.add_output ~output:(Int (int_of_float r))
  else state |> State.add_output ~output:(Float r)

and eval_binary_pow ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let r =
    match (a, b) with
    | Int a, Int b ->
        float_of_int a ** float_of_int b
    | Float a, Float b ->
        a ** b
    | Float a, Int b ->
        a ** float_of_int b
    | Int a, Float b ->
        float_of_int a ** b
    | _ ->
        failwith "Trying to raise non numeric literals."
  in
  if Float.is_integer r then
    state |> State.add_output ~output:(Int (int_of_float r))
  else state |> State.add_output ~output:(Float r)

and eval_binary_modulo ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let ( %. ) = mod_float in
  let ( % ) = ( mod ) in
  let r =
    match (a, b) with
    | Int _, Int 0 ->
        failwith "Trying to modulo with 0 on right hand side."
    | Int _, Float 0. ->
        failwith "Trying to modulo with 0 on right hand side."
    | Float _, Float 0. ->
        failwith "Trying to modulo with 0 on right hand side."
    | Float _, Int 0 ->
        failwith "Trying to modulo with 0 on right hand side."
    | Int a, Int b ->
        a % b
    | Float a, Float b ->
        int_of_float (a %. b)
    | Float a, Int b ->
        int_of_float a % b
    | Int a, Float b ->
        a % int_of_float b
    | _ ->
        failwith "Trying to modulo non numeric literals."
  in
  state |> State.add_output ~output:(Int r)

and eval_binary_and ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.is_true a && Value.is_true b))

and eval_binary_or ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.is_true a || Value.is_true b))

and eval_binary_less ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.compare a b < 0))

and eval_binary_less_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.compare a b <= 0))

and eval_binary_greater ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.compare a b > 0))

and eval_binary_greater_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.compare a b >= 0))

and eval_binary_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (Value.equal a b))

and eval_binary_not_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(Bool (not (Value.equal a b)))

and eval_binary_concat ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match (a, b) with
  | String a, String b ->
      state |> State.add_output ~output:(String (a ^ b))
  | _ ->
      failwith "Trying to concat non string literals."

and eval_binary_dot_access ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  match (left, right) with
  | Null, _ ->
      state |> State.add_output ~output:Null
  | Record left, Ast.LowercaseIdentifierExpression (Lowercase_Id b) ->
      let output = left |> StringMap.find_opt b |> Option.value ~default:Null in
      state |> State.add_output ~output
  | ( HtmlTemplateNode (tag, attributes, _children, _self_closing)
    , Ast.LowercaseIdentifierExpression (Lowercase_Id b) ) -> (
    match b with
    | "tag" ->
        state |> State.add_output ~output:(String tag)
    | "attributes" ->
        state |> State.add_output ~output:(Record attributes)
    | s ->
        failwith
          ( "Unknown property " ^ s
          ^ " on template node. Known properties are: `tag` and `attributes`."
          ) )
  | ( ComponentTemplateNode (_, tag, attributes)
    , Ast.LowercaseIdentifierExpression (Lowercase_Id b) ) -> (
    match b with
    | "tag" ->
        state |> State.add_output ~output:(String tag)
    | "attributes" ->
        state |> State.add_output ~output:(Record attributes)
    | s ->
        failwith
          ( "Unknown property " ^ s
          ^ " on component. Known properties are: `tag` and`attributes`." ) )
  | Record _, _ ->
      failwith
        "Expected right hand side of record access to be a lowercase \
         identifier."
  | ( DefinitionInfo (name, maybe_library, _)
    , Ast.LowercaseIdentifierExpression (Lowercase_Id b) ) -> (
    match
      (state |> State.get_used_values |> List.assoc_opt name, maybe_library)
    with
    | None, Some (`Library (top_level_bindings, _))
    | Some (DefinitionInfo (_, Some (`Library (top_level_bindings, _)), _)), _
      ->
        state
        |> State.add_output
             ~output:
               ( top_level_bindings |> List.assoc_opt b
               |> Option.map (fun b -> b.value)
               |> Option.value ~default:Null )
    | None, None ->
        state |> State.add_output ~output:Null
    | _ ->
        failwith
          "Trying to access a property on a non record, library or template \
           value." )
  | ( DefinitionInfo (name, maybe_library, _)
    , Ast.UppercaseIdentifierExpression (Uppercase_Id b) ) -> (
    match
      (state |> State.get_used_values |> List.assoc_opt name, maybe_library)
    with
    | None, Some (`Library (_, use_scope))
    | Some (DefinitionInfo (_, Some (`Library (_, use_scope)), _)), _ ->
        state
        |> State.add_output
             ~output:
               (use_scope |> List.assoc_opt b |> Option.value ~default:Null)
    | None, None ->
        state |> State.add_output ~output:Null
    | _ ->
        failwith
          "Trying to access a property on a non record, library or template \
           value." )
  | DefinitionInfo (name, None, _), _ ->
      failwith
        ( "Trying to access a property on a non existant library `" ^ name
        ^ "` ." )
  | _, Ast.LowercaseIdentifierExpression _ ->
      failwith
        "Trying to access a property on a non record, library or template \
         value."
  | _ ->
      failwith "I am really not sure what you are trying to do here..."

and eval_binary_bracket_access ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match (left, right) with
  | Array left, Int right ->
      let output = try left.(right) with Invalid_argument _ -> Null in
      state |> State.add_output ~output
  | Record left, String right ->
      let output =
        left |> StringMap.find_opt right |> Option.value ~default:Null
      in
      state |> State.add_output ~output
  | Null, _ ->
      state |> State.add_output ~output:Null
  | Array _, _ ->
      failwith "Cannot access array with a non integer value."
  | Record _, _ ->
      failwith "Cannot access record with a non string value."
  | _ ->
      failwith "Trying to access a property on a non record or array value."

and eval_binary_array_add ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match (left, right) with
  | Array left, value ->
      state |> State.add_output ~output:(Array (Array.append left [|value|]))
  | _ ->
      failwith "Trying to add an element on a non array value."

and eval_binary_merge ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match (left, right) with
  | Array left, Array right ->
      state |> State.add_output ~output:(Array (Array.append left right))
  | Record left, Record right ->
      state
      |> State.add_output
           ~output:
             (Record (StringMap.union (fun _key _x y -> Some y) left right))
  | HtmlTemplateNode (tag, attributes, children, self_closing), Record right ->
      let attributes =
        StringMap.union (fun _key _x y -> Some y) attributes right
      in
      state
      |> State.add_output
           ~output:(HtmlTemplateNode (tag, attributes, children, self_closing))
  | HtmlTemplateNode _, _ ->
      failwith "Trying to merge a non record value onto tag attributes."
  | ComponentTemplateNode (fn, tag, attributes), Record right ->
      let attributes =
        StringMap.union (fun _key _x y -> Some y) attributes right
      in
      state
      |> State.add_output ~output:(ComponentTemplateNode (fn, tag, attributes))
  | ComponentTemplateNode _, _ ->
      failwith "Trying to merge a non record value onto tag attributes."
  | Array _, _ ->
      failwith "Trying to merge a non array value onto an array."
  | _, Array _ ->
      failwith "Trying to merge an array value onto a non array."
  | _ ->
      failwith "Trying to merge two non array values."

and eval_unary_not ~state expression =
  match eval_expression ~state expression |> State.get_output with
  | DefinitionInfo (name, typ, negated) ->
      let negated =
        match negated with `Negated -> `NotNegated | `NotNegated -> `Negated
      in
      state |> State.add_output ~output:(DefinitionInfo (name, typ, negated))
  | v ->
      state |> State.add_output ~output:(Bool (not (Value.is_true v)))

and eval_unary_minus ~state expression =
  match eval_expression ~state expression |> State.get_output with
  | Int i ->
      state |> State.add_output ~output:(Int (Int.neg i))
  | Float f ->
      state |> State.add_output ~output:(Float (Float.neg f))
  | _ ->
      failwith
        "Invalid usage of unary `-` operator. You are only able to negate \
         integers or floats."

and eval_lowercase_identifier ~state ident =
  state
  |> State.get_value_from_scope ~ident
  |> function
  | None ->
      failwith ("Unbound identifier `" ^ ident ^ "`")
  | Some {value; is_mutable= _; is_optional= _} ->
      state |> State.add_output ~output:value

and eval_let ~state ~ident ~is_mutable ~is_optional expression =
  let state =
    expression
    |> eval_expression
         ~state:{state with binding_identifier= Some (is_optional, ident)}
  in
  match State.get_output state with
  | Null when not is_optional ->
      failwith
        ( "identifier " ^ ident
        ^ " is not marked as nullable, but was given a null value." )
  | value ->
      state
      |> State.add_value_to_scope ~ident ~value ~is_mutable ~is_optional
      |> State.add_output ~output:Null

and eval_use ~state ~ident expression =
  let value = expression |> eval_expression ~state |> State.get_output in
  match value with
  | value ->
      state
      |> State.add_value_to_use_scope ~ident ~value
      |> State.add_output ~output:Null

and eval_mutation ~state ~ident expression =
  let current_binding = State.get_value_from_scope ~ident state in
  match current_binding with
  | None ->
      failwith
        "Trying to update a variable, which does not exist in the current \
         scope."
  | Some {is_mutable= false; _} ->
      failwith "Trying to update a non mutable variable."
  | Some {value= _; is_mutable= true; is_optional} ->
      let output =
        expression
        |> eval_expression
             ~state:{state with binding_identifier= Some (is_optional, ident)}
      in
      let () =
        output |> State.get_output
        |> function
        | Null when not is_optional ->
            failwith
              ( "identifier " ^ ident
              ^ " is not marked as nullable, but was tried to be updated with \
                 a null value." )
        | value ->
            state |> State.update_value_in_scope ~ident ~value
      in
      state |> State.add_output ~output:Null

and eval_if ~state ~condition ~alternate ~consequent =
  let condition_matches =
    condition |> eval_expression ~state |> State.get_output |> Value.is_true
  in
  match (condition_matches, alternate) with
  | true, _ ->
      consequent |> eval_statement ~state
  | false, Some alt ->
      alt |> eval_statement ~state
  | false, None ->
      state |> State.add_output ~output:Null

and eval_for_in ~state ~index_ident ~ident ~reverse ~iterable body =
  let make_rev array =
    array |> Array.stable_sort (fun _ _ -> 1) ;
    array
  in
  let iterable = iterable |> eval_expression ~state |> State.get_output in
  let maybe_rev = if reverse then make_rev else fun arr -> arr in
  let index = ref (-1) in
  let rec loop acc = function
    | [] ->
        List.rev acc
    | value :: tl -> (
        index := succ !index ;
        let state =
          state
          |> State.add_value_to_scope ~ident ~value ~is_mutable:false
               ~is_optional:false
        in
        let state =
          match index_ident with
          | Some (Lowercase_Id ident) ->
              state
              |> State.add_value_to_scope ~ident ~value:(Int !index)
                   ~is_mutable:false ~is_optional:false
          | None ->
              state
        in
        match eval_statement ~state body with
        | exception Loop_Continue ->
            loop acc tl
        | exception Loop_Break ->
            List.rev acc
        | v ->
            loop (State.get_output v :: acc) tl )
  in
  match iterable with
  | Array l ->
      let res = l |> maybe_rev |> Array.to_list |> loop [] |> Value.of_list in
      state |> State.add_output ~output:res
  | String s ->
      let res =
        s |> String.to_seq |> Array.of_seq |> maybe_rev
        |> Array.map (fun c -> String (String.make 1 c))
        |> Array.to_list |> loop [] |> Value.of_list
      in
      state |> State.add_output ~output:res
  | Null ->
      state |> State.add_output ~output:Null
  | HtmlTemplateNode _ ->
      failwith "Cannot iterate over template node"
  | ComponentTemplateNode _ ->
      failwith "Cannot iterate over template node"
  | Record _ ->
      failwith "Cannot iterate over record value"
  | Int _ ->
      failwith "Cannot iterate over int value"
  | Float _ ->
      failwith "Cannot iterate over float value"
  | Bool _ ->
      failwith "Cannot iterate over boolean value"
  | DefinitionInfo _ ->
      failwith "Cannot iterate over definition info"
  | Function _ ->
      failwith "Cannot iterate over function"
  | TagInfo _ ->
      should_never_happen ()

and eval_range ~state ~inclusive from upto =
  let from = from |> eval_expression ~state |> State.get_output in
  let upto = upto |> eval_expression ~state |> State.get_output in
  let from, upto =
    match (from, upto) with
    | Int from, Int upto ->
        (from, upto)
    | Int _, _ ->
        failwith
          "Can't construct range in for loop. The end of your range is not of \
           type int."
    | _, Int _ ->
        failwith
          "Can't construct range in for loop. The start of your range is not \
           of type int."
    | _, _ ->
        failwith
          "Can't construct range in for loop. The start and end of your range \
           are not of type int."
  in
  let iter =
    if from > upto then [||]
    else
      let start = from in
      let stop = if not inclusive then upto else upto + 1 in
      Array.init (stop - start) (fun i -> Int (i + start))
  in
  state |> State.add_output ~output:(Array iter)

and eval_block ~state statements =
  let state = state |> State.add_scope in
  statements |> List.fold_left (fun state -> eval_statement ~state) state

and eval_tag ~state tag =
  let Ast.{tag_name; attributes; transformer} = tag in
  let is_optional =
    state.binding_identifier |> Option.map fst |> Option.value ~default:false
  in
  let attributes =
    attributes
    |> StringMap.map (fun it ->
           it
           |> eval_expression ~state:{state with tag_info= true}
           |> State.get_output )
  in
  let apply_transformer ~transformer value =
    match transformer with
    | Some (Ast.Lowercase_Id ident, expr) ->
        let state =
          state |> State.add_scope
          |> State.add_value_to_scope ~ident ~value ~is_optional:false
               ~is_mutable:false
        in
        eval_expression ~state expr |> State.get_output
    | _ ->
        value
  in
  let attributes =
    match (StringMap.find_opt "key" attributes, state.binding_identifier) with
    | None, Some ident ->
        attributes |> StringMap.add "key" (String (snd ident))
    | Some (String _), _ ->
        attributes
    | Some _, _ ->
        failwith "Expected attribute `key` on tag to be of type string"
    | None, None ->
        attributes
  in
  let tag =
    (tag_name, is_optional, attributes, apply_transformer ~transformer)
  in
  let value =
    match state.tag_info with
    | false ->
        state |> State.call_tag_listener ~key:("#" ^ tag_name) ~tag
    | true ->
        TagInfo tag
  in
  state |> State.add_output ~output:value

and eval_template ~state template =
  match template with
  | Ast.TextTemplateNode text ->
      state |> State.add_output ~output:(String text)
  | Ast.HtmlTemplateNode {tag; attributes; children; self_closing} ->
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
           ~output:(HtmlTemplateNode (tag, attributes, children, self_closing))
  | Ast.ExpressionTemplateNode expr ->
      eval_expression ~state expr
  | Ast.ComponentTemplateNode
      {identifier= Uppercase_Id tag; attributes; children} ->
      let attributes =
        attributes
        |> StringMap.map (eval_expression ~state)
        |> StringMap.map State.get_output
      in
      let children =
        children |> List.map (eval_template ~state) |> List.map State.get_output
      in
      (* TODO: Remove this, as we are not really changing anything anymore... *)
      let component_tag_listeners =
        StringMap.empty
        |> StringMap.add "#String" Pinc_Tags.Default.string
        |> StringMap.add "#Int" Pinc_Tags.Default.int
        |> StringMap.add "#Float" Pinc_Tags.Default.float
        |> StringMap.add "#Boolean" Pinc_Tags.Default.boolean
        |> StringMap.add "#Array" Pinc_Tags.Default.array
        |> StringMap.add "#Record" Pinc_Tags.Default.record
        |> StringMap.add "#Slot" Pinc_Tags.Default.slot
      in
      let tag_listeners =
        StringMap.union
          (fun _key _x y -> Some y)
          state.tag_listeners component_tag_listeners
      in
      let render_fn attributes =
        let state =
          State.make
            ~parent_component:(tag, attributes, children)
            ~tag_listeners ~context:state.context state.declarations
        in
        state.declarations |> StringMap.find_opt tag
        |> (function
             | Some declaration ->
                 eval_declaration ~state declaration
             | None ->
                 failwith ("Declaration with name `" ^ tag ^ "` was not found.")
             )
        |> State.get_output
      in
      state
      |> State.add_output
           ~output:(ComponentTemplateNode (render_fn, tag, attributes))

and eval_declaration ~state declaration =
  match declaration with
  | Ast.ComponentDeclaration (_attrs, body)
  | Ast.LibraryDeclaration (_attrs, body)
  | Ast.SiteDeclaration (_attrs, body)
  | Ast.PageDeclaration (_attrs, body)
  | Ast.StoreDeclaration (_attrs, body) ->
      eval_expression ~state body

let eval ?tag_listeners ~root declarations =
  let tag_listeners =
    let default_tag_listeners =
      StringMap.empty
      |> StringMap.add "#SetContext" Pinc_Tags.Default.set_context
      |> StringMap.add "#GetContext" Pinc_Tags.Default.get_context
    in
    match tag_listeners with
    | None ->
        default_tag_listeners
    | Some listeners ->
        StringMap.union
          (fun _key _x y -> Some y)
          listeners default_tag_listeners
  in
  let state = State.make ~tag_listeners declarations in
  declarations |> StringMap.find_opt root
  |> function
  | Some declaration ->
      eval_declaration ~state declaration
  | None ->
      failwith ("Declaration with name `" ^ root ^ "` was not found.")

let from_source ?(filename = "") ~source root =
  let declarations = Parser.parse ~filename source in
  eval ~root declarations
