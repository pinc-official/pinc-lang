open Pinc_Interpreter_Types
module Ast = Pinc_Frontend.Ast
module Parser = Pinc_Frontend.Parser
module Location = Pinc_Diagnostics.Location

exception Loop_Break of state
exception Loop_Continue of state

module Value = struct
  let null ~value_loc () = { value_loc; value_desc = Null }
  let of_char ~value_loc c = { value_loc; value_desc = Char c }
  let of_string ~value_loc s = { value_loc; value_desc = String s }
  let of_bool ~value_loc b = { value_loc; value_desc = Bool b }
  let of_int ~value_loc i = { value_loc; value_desc = Int i }
  let of_float ~value_loc f = { value_loc; value_desc = Float f }
  let of_array ~value_loc l = { value_loc; value_desc = Array l }
  let of_list ~value_loc l = { value_loc; value_desc = Array (Array.of_list l) }
  let of_string_map ~value_loc m = { value_loc; value_desc = Record m }

  let make_component ~render ~tag ~attributes =
    let result = render attributes in
    {
      value_loc = Location.none;
      value_desc = ComponentTemplateNode (render, tag, attributes, result);
    }
  ;;

  let rec to_string value =
    match value.value_desc with
    | Portal list -> list |> List.rev_map to_string |> String.concat "\n"
    | Null -> ""
    | String s -> s
    | Char c ->
        let buf = Buffer.create 32 in
        c |> Buffer.add_utf_8_uchar buf;
        Buffer.contents buf
    | Int i -> string_of_int i
    | Float f when Float.is_integer f -> string_of_int (int_of_float f)
    | Float f -> string_of_float f
    | Bool b ->
        if b then
          "true"
        else
          "false"
    | Array l ->
        let buf = Buffer.create 200 in
        l
        |> Array.iteri (fun i it ->
               if i <> 0 then
                 Buffer.add_char buf '\n';
               Buffer.add_string buf (to_string it));
        Buffer.contents buf
    | Record m ->
        let b = Buffer.create 1024 in
        m
        |> StringMap.to_seq
        |> Seq.iter (fun (_key, (_index, value)) ->
               Buffer.add_string b (to_string value);
               Buffer.add_char b '\n');
        Buffer.contents b
    | HtmlTemplateNode (tag, attributes, children, self_closing) ->
        let buf = Buffer.create 128 in
        Buffer.add_char buf '<';
        Buffer.add_string buf tag;
        if not (StringMap.is_empty attributes) then
          attributes
          |> StringMap.iter (fun key value ->
                 match value.value_desc with
                 | Null -> ()
                 | Portal _
                 | Function _
                 | String _
                 | Int _
                 | Char _
                 | Float _
                 | Bool _
                 | Array _
                 | Record _
                 | HtmlTemplateNode _
                 | ComponentTemplateNode _
                 | DefinitionInfo _ ->
                     Buffer.add_char buf ' ';
                     Buffer.add_string buf key;
                     Buffer.add_char buf '=';
                     Buffer.add_char buf '"';
                     Buffer.add_string buf (value |> to_string);
                     Buffer.add_char buf '"'
                 | TagInfo _ -> assert false);
        if self_closing && Pinc_HTML.is_void_el tag then
          Buffer.add_string buf " />"
        else (
          Buffer.add_char buf '>';
          children |> List.iter (fun child -> Buffer.add_string buf (to_string child));
          Buffer.add_char buf '<';
          Buffer.add_char buf '/';
          Buffer.add_string buf tag;
          Buffer.add_char buf '>');
        Buffer.contents buf
    | ComponentTemplateNode (_render_fn, _tag, _attributes, result) -> result |> to_string
    | Function _ -> ""
    | DefinitionInfo _ -> ""
    | TagInfo _ -> assert false
  ;;

  let is_true value =
    match value.value_desc with
    | Null -> false
    | Bool b -> b
    | String s -> s |> String.trim |> String.length > 0
    | Char _ -> true
    | Int _ -> true
    | Float _ -> true
    | HtmlTemplateNode _ -> true
    | ComponentTemplateNode _ -> true
    | Portal _ -> false
    | DefinitionInfo (_name, Some _, _negated) -> true
    | DefinitionInfo (_name, None, _negated) -> false
    | Function _ -> true
    | Array [||] -> false
    | Array _ -> true
    | Record m -> not (StringMap.is_empty m)
    | TagInfo _ -> assert false
  ;;

  let rec equal a b =
    match (a.value_desc, b.value_desc) with
    | String a, String b -> String.equal a b
    | Char a, Char b -> Uchar.equal a b
    | Int a, Int b -> a = b
    | Float a, Float b -> a = b
    | Float a, Int b -> a = float_of_int b
    | Int a, Float b -> float_of_int a = b
    | Bool a, Bool b -> a = b
    | Array a, Array b -> Array.combine a b |> Array.for_all (fun (a, b) -> equal a b)
    | Record a, Record b -> StringMap.equal (fun (_, a) (_, b) -> equal a b) a b
    | Function _, Function _ -> false
    | DefinitionInfo (a, _, _), DefinitionInfo (b, _, _) -> String.equal a b
    | ( HtmlTemplateNode (a_tag, a_attrs, a_children, a_self_closing),
        HtmlTemplateNode (b_tag, b_attrs, b_children, b_self_closing) ) ->
        a_tag = b_tag
        && a_self_closing = b_self_closing
        && StringMap.equal equal a_attrs b_attrs
        && a_children = b_children
    | ( ComponentTemplateNode (_, a_tag, a_attributes, _),
        ComponentTemplateNode (_, b_tag, b_attributes, _) ) ->
        a_tag = b_tag && StringMap.equal equal a_attributes b_attributes
    | Null, Null -> true
    | TagInfo _, _ -> assert false
    | _, TagInfo _ -> assert false
    | _ -> false
  ;;

  let compare a b =
    match (a.value_desc, b.value_desc) with
    | String a, String b -> String.compare a b
    | Char a, Char b -> Uchar.compare a b
    | Char a, Int b -> Int.compare (Uchar.to_int a) b
    | Int a, Char b -> Int.compare a (Uchar.to_int b)
    | Int a, Int b -> Int.compare a b
    | Float a, Float b -> Float.compare a b
    | Float a, Int b -> Float.compare a (float_of_int b)
    | Int a, Float b -> Float.compare (float_of_int a) b
    | Bool a, Bool b -> Bool.compare a b
    | Array a, Array b -> Int.compare (Array.length a) (Array.length b)
    | Record a, Record b -> StringMap.compare compare a b
    | Null, Null -> 0
    | ComponentTemplateNode _, ComponentTemplateNode _ -> 0
    | HtmlTemplateNode _, HtmlTemplateNode _ -> 0
    | DefinitionInfo _, DefinitionInfo _ -> 0
    | Function _, Function _ -> 0
    | TagInfo _, _ -> assert false
    | _, TagInfo _ -> assert false
    | Portal _, _ -> 0
    | _, Portal _ -> 0
    | _ -> 0
  ;;
end

module State = struct
  let make
      ?(tag_listeners = Hashtbl.create 10)
      ?parent_component
      ?(context = Hashtbl.create 10)
      ?(portals = Hashtbl.create 10)
      ?(tag_cache = Hashtbl.create 10)
      ~mode
      declarations =
    {
      mode;
      binding_identifier = None;
      declarations;
      output = { value_desc = Null; value_loc = Location.none };
      environment = { scope = []; use_scope = [] };
      tag_listeners;
      parent_tag = None;
      tag_cache;
      parent_component;
      context;
      portals;
    }
  ;;

  let add_scope t =
    let environment = { t.environment with scope = [] :: t.environment.scope } in
    { t with environment }
  ;;

  let add_value_to_scope ~ident ~value ~is_optional ~is_mutable t =
    let update_scope t =
      match t.environment.scope with
      | [] -> assert false
      | scope :: rest -> ((ident, { is_mutable; is_optional; value }) :: scope) :: rest
    in
    let environment = { t.environment with scope = update_scope t } in
    { t with environment }
  ;;

  let add_value_to_use_scope ~ident ~value t =
    let use_scope = (ident, value) :: t.environment.use_scope in
    let environment = { t.environment with use_scope } in
    { t with environment }
  ;;

  let update_value_in_scope ~ident ~value t =
    let updated = ref false in
    let rec update_scope state =
      state.environment.scope
      |> List.map
           (List.map (function
               | key, binding when key = ident && binding.is_mutable ->
                   updated := true;
                   (key, { binding with value })
               | ( key,
                   ({
                      value =
                        {
                          value_desc = Function { state = fn_state; parameters; exec };
                          _;
                        };
                      _;
                    } as binding) )
                 when not !updated ->
                   fn_state.environment.scope <- update_scope fn_state;
                   ( key,
                     {
                       binding with
                       value =
                         {
                           value with
                           value_desc = Function { state = fn_state; parameters; exec };
                         };
                     } )
               | v -> v))
    in
    let new_scope = update_scope t in
    t.environment.scope <- new_scope
  ;;

  let add_value_to_function_scopes ~ident ~value ~is_optional ~is_mutable t =
    let update_scope state =
      List.map
        (List.map (function
            | ( key,
                ({ value = { value_desc = Function { state; parameters; exec }; _ }; _ }
                as binding) ) ->
                let new_state =
                  add_value_to_scope ~ident ~value ~is_optional ~is_mutable state
                in
                ( key,
                  {
                    binding with
                    value =
                      {
                        value with
                        value_desc = Function { state = new_state; parameters; exec };
                      };
                  } )
            | v -> v))
        state.environment.scope
    in
    t.environment.scope <- update_scope t
  ;;

  let get_value_from_scope ~ident t =
    t.environment.scope |> List.find_map (List.assoc_opt ident)
  ;;

  let get_output t = t.output
  let add_output ~output t = { t with output }
  let get_bindings t = t.environment.scope |> List.hd
  let get_used_values t = t.environment.use_scope

  let get_parent_component t =
    t.parent_component
    |> Option.map (fun (name, attributes, children) -> (name, attributes, children))
  ;;
end

let rec eval_statement ~state statement =
  match statement.Ast.statement_desc with
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
  | Ast.UseStatement (Uppercase_Id ident, expression) -> eval_use ~state ~ident expression
  | Ast.BreakStatement _ -> raise_notrace (Loop_Break state)
  | Ast.ContinueStatement _ -> raise_notrace (Loop_Continue state)
  | Ast.ExpressionStatement expression -> expression |> eval_expression ~state

and eval_expression ~state expression =
  match expression.expression_desc with
  | Ast.Comment -> state
  | Ast.Char c ->
      state
      |> State.add_output ~output:(Value.of_char ~value_loc:expression.expression_loc c)
  | Ast.Int i ->
      state
      |> State.add_output ~output:(Value.of_int ~value_loc:expression.expression_loc i)
  | Ast.Float f when Float.is_integer f ->
      state
      |> State.add_output
           ~output:(Value.of_int ~value_loc:expression.expression_loc (int_of_float f))
  | Ast.Float f ->
      state
      |> State.add_output ~output:(Value.of_float ~value_loc:expression.expression_loc f)
  | Ast.Bool b ->
      state
      |> State.add_output ~output:(Value.of_bool ~value_loc:expression.expression_loc b)
  | Ast.Array l ->
      state
      |> State.add_output
           ~output:
             (l
             |> Array.map (fun it -> it |> eval_expression ~state |> State.get_output)
             |> Value.of_array ~value_loc:expression.expression_loc)
  | Ast.Record map ->
      state
      |> State.add_output
           ~output:
             (map
             |> StringMap.mapi (fun ident attr ->
                    let index, optional, expression = attr in
                    expression
                    |> eval_expression
                         ~state:{ state with binding_identifier = Some (optional, ident) }
                    |> State.get_output
                    |> function
                    | { value_desc = Null; value_loc } when not optional ->
                        Pinc_Diagnostics.error
                          value_loc
                          (Printf.sprintf
                             "identifier %s is not marked as nullable, but was given a \
                              null value."
                             ident)
                    | value -> (index, value))
             |> Value.of_string_map ~value_loc:expression.expression_loc)
  | Ast.String template -> eval_string_template ~state template
  | Ast.Function { parameters; body } ->
      eval_function_declaration ~loc:expression.expression_loc ~state ~parameters body
  | Ast.FunctionCall { function_definition; arguments } ->
      eval_function_call ~state ~arguments function_definition
  | Ast.UppercaseIdentifierExpression id ->
      let declaration = state.declarations |> StringMap.find_opt id in
      let typ =
        match declaration with
        | None -> None
        | Some { declaration_type = Ast.Declaration_Component _; _ } -> Some `Component
        | Some { declaration_type = Ast.Declaration_Page _; _ } -> Some `Page
        | Some { declaration_type = Ast.Declaration_Site _; _ } -> Some `Site
        | Some { declaration_type = Ast.Declaration_Store _; _ } -> Some `Store
        | Some { declaration_type = Ast.Declaration_Library _; _ } ->
            let state =
              State.make
                ~tag_listeners:state.tag_listeners
                ~context:state.context
                ~portals:state.portals
                ~tag_cache:state.tag_cache
                ~mode:state.mode
                state.declarations
            in
            let declaration = eval_declaration ~state id in
            let top_level_bindings = declaration |> State.get_bindings in
            let used_values = declaration |> State.get_used_values in
            Some (`Library (top_level_bindings, used_values))
      in
      state
      |> State.add_output
           ~output:
             {
               value_desc = DefinitionInfo (id, typ, `NotNegated);
               value_loc = expression.expression_loc;
             }
  | Ast.LowercaseIdentifierExpression id ->
      eval_lowercase_identifier ~loc:expression.expression_loc ~state id
  | Ast.TagExpression tag -> eval_tag ~state tag
  | Ast.ForInExpression { index; iterator = Lowercase_Id ident; reverse; iterable; body }
    -> eval_for_in ~state ~index_ident:index ~ident ~reverse ~iterable body
  | Ast.TemplateExpression nodes ->
      state
      |> State.add_output
           ~output:
             (nodes
             |> List.map (fun it -> it |> eval_template ~state |> State.get_output)
             |> Value.of_list ~value_loc:expression.expression_loc)
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
  let start_loc = ref Location.none in
  let end_loc = ref Location.none in
  state
  |> State.add_output
       ~output:
         (template
         |> List.mapi (fun index string_template ->
                if index = 0 then
                  start_loc := string_template.Ast.string_template_loc
                else
                  end_loc := string_template.Ast.string_template_loc;

                match string_template.Ast.string_template_desc with
                | StringText s -> s
                | StringInterpolation e ->
                    eval_expression ~state e |> State.get_output |> Value.to_string)
         |> String.concat ""
         |> Value.of_string
              ~value_loc:(Location.make ~s:!start_loc.loc_start ~e:!end_loc.loc_end ()))

and eval_function_declaration ~state ~loc ~parameters body =
  let ident = state.binding_identifier in
  let self = ref { value_desc = Null; value_loc = loc } in
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
  let fn = { value_loc = loc; value_desc = Function { parameters; state; exec } } in
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

and eval_function_call ~state ~arguments function_definition =
  let maybe_fn = eval_expression ~state function_definition |> State.get_output in
  match maybe_fn.value_desc with
  | Function { parameters; state = fn_state; exec }
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
  | Function { parameters; state = _; exec = _ } ->
      if List.compare_lengths parameters arguments > 0 then (
        let arguments_len = List.length arguments in
        let missing =
          parameters
          |> List.filteri (fun index _ -> index > arguments_len - 1)
          |> List.map (fun item -> "`" ^ item ^ "`")
          |> String.concat ", "
        in
        Pinc_Diagnostics.error
          function_definition.expression_loc
          ("This function was provided too few arguments. The following parameters are \
            missing: "
          ^ missing))
      else
        Pinc_Diagnostics.error
          function_definition.expression_loc
          ("This function only accepts "
          ^ string_of_int (List.length parameters)
          ^ " arguments, but was provided "
          ^ string_of_int (List.length arguments)
          ^ " here.")
  | _ ->
      Pinc_Diagnostics.error
        function_definition.expression_loc
        "Trying to call a non function value"

and eval_binary_pipe ~state left right =
  let right =
    match right with
    | Ast.
        {
          expression_desc = FunctionCall { function_definition; arguments };
          expression_loc;
        } ->
        let expression_desc =
          Ast.FunctionCall { function_definition; arguments = left :: arguments }
        in
        Ast.{ expression_desc; expression_loc }
    | fn ->
        let expression_desc =
          Ast.FunctionCall { function_definition = fn; arguments = [ left ] }
        in
        { fn with expression_desc }
  in
  right |> eval_expression ~state

and eval_binary_plus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc =
    Location.merge ~s:left.expression_loc ~e:right.expression_loc ()
  in
  match (a.value_desc, b.value_desc) with
  | Char a, Char b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char
                ~value_loc:merged_value_loc
                Uchar.(of_int (to_int a + to_int b)))
  | Char a, Int b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char ~value_loc:merged_value_loc Uchar.(of_int (to_int a + b)))
  | Int a, Char b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char ~value_loc:merged_value_loc Uchar.(of_int (a + to_int b)))
  | Int a, Int b ->
      state |> State.add_output ~output:(Value.of_int ~value_loc:merged_value_loc (a + b))
  | Float a, Float b ->
      state
      |> State.add_output ~output:(Value.of_float ~value_loc:merged_value_loc (a +. b))
  | Float a, Int b ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:merged_value_loc (a +. float_of_int b))
  | Int a, Float b ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:merged_value_loc (float_of_int a +. b))
  | (Int _ | Float _), _ ->
      Pinc_Diagnostics.error right.expression_loc "Trying to add non numeric literals."
  | _, (Int _ | Float _) ->
      Pinc_Diagnostics.error left.expression_loc "Trying to add non numeric literals."
  | _ ->
      Pinc_Diagnostics.error
        (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
        "Trying to add non numeric literals."

and eval_binary_minus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  match (a.value_desc, b.value_desc) with
  | Char a, Char b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char
                ~value_loc:merged_value_loc
                Uchar.(of_int (to_int a - to_int b)))
  | Char a, Int b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char ~value_loc:merged_value_loc Uchar.(of_int (to_int a - b)))
  | Int a, Char b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char ~value_loc:merged_value_loc Uchar.(of_int (a - to_int b)))
  | Int a, Int b ->
      state |> State.add_output ~output:(Value.of_int ~value_loc:merged_value_loc (a - b))
  | Float a, Float b ->
      state
      |> State.add_output ~output:(Value.of_float ~value_loc:merged_value_loc (a -. b))
  | Float a, Int b ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:merged_value_loc (a -. float_of_int b))
  | Int a, Float b ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:merged_value_loc (float_of_int a -. b))
  | (Int _ | Float _), _ ->
      Pinc_Diagnostics.error b.value_loc "Trying to subtract non numeric literals."
  | _, (Int _ | Float _) ->
      Pinc_Diagnostics.error a.value_loc "Trying to subtract non numeric literals."
  | _ ->
      Pinc_Diagnostics.error merged_value_loc "Trying to subtract non numeric literals."

and eval_binary_times ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  match (a.value_desc, b.value_desc) with
  | Char a, Char b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char
                ~value_loc:merged_value_loc
                Uchar.(of_int (to_int a * to_int b)))
  | Char a, Int b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char ~value_loc:merged_value_loc Uchar.(of_int (to_int a * b)))
  | Int a, Char b ->
      state
      |> State.add_output
           ~output:
             (Value.of_char ~value_loc:merged_value_loc Uchar.(of_int (a * to_int b)))
  | Int a, Int b ->
      state |> State.add_output ~output:(Value.of_int ~value_loc:merged_value_loc (a * b))
  | Float a, Float b ->
      state
      |> State.add_output ~output:(Value.of_float ~value_loc:merged_value_loc (a *. b))
  | Float a, Int b ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:merged_value_loc (a *. float_of_int b))
  | Int a, Float b ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:merged_value_loc (float_of_int a *. b))
  | (Int _ | Float _), _ ->
      Pinc_Diagnostics.error b.value_loc "Trying to multiply non numeric literals."
  | _, (Int _ | Float _) ->
      Pinc_Diagnostics.error a.value_loc "Trying to multiply non numeric literals."
  | _ ->
      Pinc_Diagnostics.error merged_value_loc "Trying to multiply non numeric literals."

and eval_binary_div ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  let r =
    match (a.value_desc, b.value_desc) with
    | Int _, Int 0 | Float _, Float 0. | Float _, Int 0 | Int _, Float 0. ->
        Pinc_Diagnostics.error b.value_loc "Trying to divide by 0"
    | Int a, Int b -> float_of_int a /. float_of_int b
    | Float a, Float b -> a /. b
    | Float a, Int b -> a /. float_of_int b
    | Int a, Float b -> float_of_int a /. b
    | (Int _ | Float _), _ ->
        Pinc_Diagnostics.error b.value_loc "Trying to divide non numeric literals."
    | _, (Int _ | Float _) ->
        Pinc_Diagnostics.error a.value_loc "Trying to divide non numeric literals."
    | _ ->
        Pinc_Diagnostics.error merged_value_loc "Trying to divide non numeric literals."
  in

  if Float.is_integer r then
    state
    |> State.add_output
         ~output:(Value.of_int ~value_loc:merged_value_loc (int_of_float r))
  else
    state |> State.add_output ~output:(Value.of_float ~value_loc:merged_value_loc r)

and eval_binary_pow ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  let r =
    match (a.value_desc, b.value_desc) with
    | Int a, Int b -> float_of_int a ** float_of_int b
    | Float a, Float b -> a ** b
    | Float a, Int b -> a ** float_of_int b
    | Int a, Float b -> float_of_int a ** b
    | (Int _ | Float _), _ ->
        Pinc_Diagnostics.error b.value_loc "Trying to raise non numeric literals."
    | _, (Int _ | Float _) ->
        Pinc_Diagnostics.error a.value_loc "Trying to raise non numeric literals."
    | _ -> Pinc_Diagnostics.error merged_value_loc "Trying to raise non numeric literals."
  in

  if Float.is_integer r then
    state
    |> State.add_output
         ~output:(Value.of_int ~value_loc:merged_value_loc (int_of_float r))
  else
    state |> State.add_output ~output:(Value.of_float ~value_loc:merged_value_loc r)

and eval_binary_modulo ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let ( %. ) = mod_float in
  let ( % ) = ( mod ) in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  let r =
    match (a.value_desc, b.value_desc) with
    | Int _, Int 0 | Int _, Float 0. | Float _, Float 0. | Float _, Int 0 ->
        Pinc_Diagnostics.error b.value_loc "Trying to modulo with 0 on right hand side."
    | Int a, Int b -> a % b
    | Float a, Float b -> int_of_float (a %. b)
    | Float a, Int b -> int_of_float a % b
    | Int a, Float b -> a % int_of_float b
    | (Int _ | Float _), _ ->
        Pinc_Diagnostics.error b.value_loc "Trying to modulo non numeric literals."
    | _, (Int _ | Float _) ->
        Pinc_Diagnostics.error a.value_loc "Trying to modulo non numeric literals."
    | _ ->
        Pinc_Diagnostics.error merged_value_loc "Trying to modulo non numeric literals."
  in
  state |> State.add_output ~output:(Value.of_int ~value_loc:merged_value_loc r)

and eval_binary_and ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:
         (Value.of_bool ~value_loc:merged_value_loc (Value.is_true a && Value.is_true b))

and eval_binary_or ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:
         (Value.of_bool ~value_loc:merged_value_loc (Value.is_true a || Value.is_true b))

and eval_binary_less ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Value.of_bool ~value_loc:merged_value_loc (Value.compare a b < 0))

and eval_binary_less_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Value.of_bool ~value_loc:merged_value_loc (Value.compare a b <= 0))

and eval_binary_greater ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Value.of_bool ~value_loc:merged_value_loc (Value.compare a b > 0))

and eval_binary_greater_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Value.of_bool ~value_loc:merged_value_loc (Value.compare a b >= 0))

and eval_binary_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Value.of_bool ~value_loc:merged_value_loc (Value.equal a b))

and eval_binary_not_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Value.of_bool ~value_loc:merged_value_loc (not (Value.equal a b)))

and eval_binary_concat ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  let buf = Buffer.create 32 in
  let () =
    match (a.value_desc, b.value_desc) with
    | String a, String b ->
        Buffer.add_string buf a;
        Buffer.add_string buf b
    | String a, Char b ->
        Buffer.add_string buf a;
        Buffer.add_utf_8_uchar buf b
    | Char a, String b ->
        Buffer.add_utf_8_uchar buf a;
        Buffer.add_string buf b
    | Char a, Char b ->
        Buffer.add_utf_8_uchar buf a;
        Buffer.add_utf_8_uchar buf b
    | String _, _ ->
        Pinc_Diagnostics.error b.value_loc "Trying to concat non string literals."
    | _, String _ ->
        Pinc_Diagnostics.error a.value_loc "Trying to concat non string literals."
    | _ -> Pinc_Diagnostics.error merged_value_loc "Trying to concat non string literals."
  in
  state
  |> State.add_output
       ~output:(Value.of_string ~value_loc:merged_value_loc (Buffer.contents buf))

and eval_binary_dot_access ~state left right =
  let left_value = left |> eval_expression ~state |> State.get_output in
  match (left_value.value_desc, right.expression_desc) with
  | Null, _ ->
      state |> State.add_output ~output:(Value.null ~value_loc:left_value.value_loc ())
  | Record a, Ast.LowercaseIdentifierExpression b ->
      let output =
        a
        |> StringMap.find_opt b
        |> Option.map snd
        |> Option.value ~default:(Value.null ~value_loc:left.expression_loc ())
      in
      state |> State.add_output ~output
  | HtmlTemplateNode (tag, attributes, _, _), Ast.LowercaseIdentifierExpression b -> (
      match b with
      | "tag" ->
          state
          |> State.add_output ~output:(Value.of_string ~value_loc:left.expression_loc tag)
      | "attributes" ->
          state
          |> State.add_output
               ~output:
                 (Value.of_string_map
                    ~value_loc:left.expression_loc
                    (attributes
                    |> StringMap.to_seq
                    |> Seq.mapi (fun index (key, value) -> (key, (index, value)))
                    |> StringMap.of_seq))
      | s ->
          Pinc_Diagnostics.error
            right.expression_loc
            ("Unknown property "
            ^ s
            ^ " on template node. Known properties are: `tag` and `attributes`."))
  | ComponentTemplateNode (_, tag, attributes, _), Ast.LowercaseIdentifierExpression b
    -> (
      match b with
      | "tag" ->
          state
          |> State.add_output ~output:(Value.of_string ~value_loc:left.expression_loc tag)
      | "attributes" ->
          state
          |> State.add_output
               ~output:
                 (Value.of_string_map
                    ~value_loc:left.expression_loc
                    (attributes
                    |> StringMap.to_seq
                    |> Seq.mapi (fun index (key, value) -> (key, (index, value)))
                    |> StringMap.of_seq))
      | s ->
          Pinc_Diagnostics.error
            right.expression_loc
            ("Unknown property "
            ^ s
            ^ " on component. Known properties are: `tag` and`attributes`."))
  | Record _, _ ->
      Pinc_Diagnostics.error
        right.expression_loc
        "Expected right hand side of record access to be a lowercase identifier."
  | DefinitionInfo (name, maybe_library, _), Ast.LowercaseIdentifierExpression b -> (
      match (state |> State.get_used_values |> List.assoc_opt name, maybe_library) with
      | None, Some (`Library (top_level_bindings, _))
      | ( Some
            {
              value_desc = DefinitionInfo (_, Some (`Library (top_level_bindings, _)), _);
              _;
            },
          _ ) ->
          state
          |> State.add_output
               ~output:
                 (top_level_bindings
                 |> List.assoc_opt b
                 |> Option.map (fun b -> b.value)
                 |> Option.value
                      ~default:
                        (Value.null
                           ~value_loc:
                             (Location.merge
                                ~s:left.expression_loc
                                ~e:right.expression_loc
                                ())
                           ()))
      | None, None ->
          state
          |> State.add_output
               ~output:
                 (Value.null
                    ~value_loc:
                      (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                    ())
      | _ ->
          Pinc_Diagnostics.error
            left.expression_loc
            "Trying to access a property on a non record, library or template value.")
  | DefinitionInfo (name, maybe_library, _), Ast.UppercaseIdentifierExpression b -> (
      match (state |> State.get_used_values |> List.assoc_opt name, maybe_library) with
      | None, Some (`Library (_, use_scope))
      | Some { value_desc = DefinitionInfo (_, Some (`Library (_, use_scope)), _); _ }, _
        ->
          state
          |> State.add_output
               ~output:
                 (use_scope
                 |> List.assoc_opt b
                 |> Option.value
                      ~default:
                        (Value.null
                           ~value_loc:
                             (Location.merge
                                ~s:left.expression_loc
                                ~e:right.expression_loc
                                ())
                           ()))
      | None, None ->
          state
          |> State.add_output
               ~output:
                 (Value.null
                    ~value_loc:
                      (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                    ())
      | _ ->
          Pinc_Diagnostics.error
            left.expression_loc
            "Trying to access a property on a non record, library or template value.")
  | DefinitionInfo (name, None, _), _ ->
      Pinc_Diagnostics.error
        left.expression_loc
        ("Trying to access a property on a non existant library `" ^ name ^ "`.")
  | _, Ast.LowercaseIdentifierExpression _ ->
      Pinc_Diagnostics.error
        left.expression_loc
        "Trying to access a property on a non record, library or template value."
  | _ ->
      Pinc_Diagnostics.error
        left.expression_loc
        "I am really not sure what you are trying to do here..."

and eval_binary_bracket_access ~state left right =
  let left_value = left |> eval_expression ~state |> State.get_output in
  let right_value = right |> eval_expression ~state |> State.get_output in
  match (left_value.value_desc, right_value.value_desc) with
  | Array a, Int b ->
      let output =
        try Array.get a b
        with Invalid_argument _ ->
          Value.null
            ~value_loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
            ()
      in
      state |> State.add_output ~output
  | String a, Int b ->
      let output =
        try
          String.get_utf_8_uchar a b
          |> Uchar.utf_decode_uchar
          |> Value.of_char
               ~value_loc:
                 (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
        with Invalid_argument _ ->
          Value.null
            ~value_loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
            ()
      in
      state |> State.add_output ~output
  | Record a, String b ->
      let output =
        a
        |> StringMap.find_opt b
        |> Option.map snd
        |> Option.value
             ~default:
               (Value.null
                  ~value_loc:
                    (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                  ())
      in
      state |> State.add_output ~output
  | Null, _ ->
      state
      |> State.add_output
           ~output:
             (Value.null
                ~value_loc:
                  (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                ())
  | Array _, _ ->
      Pinc_Diagnostics.error
        right.expression_loc
        "Cannot access array with a non integer value."
  | Record _, _ ->
      Pinc_Diagnostics.error
        right.expression_loc
        "Cannot access record with a non string value."
  | _ ->
      Pinc_Diagnostics.error
        left.expression_loc
        "Trying to access a property on a non record or array value."

and eval_binary_array_add ~state left right =
  let left_value = left |> eval_expression ~state |> State.get_output in
  let right_value = right |> eval_expression ~state |> State.get_output in
  match (left_value.value_desc, right_value) with
  | Array l, value ->
      let new_array = Array.append l [| value |] in
      state
      |> State.add_output
           ~output:
             (Value.of_array
                ~value_loc:
                  (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                new_array)
  | _ ->
      Pinc_Diagnostics.error
        left.expression_loc
        "Trying to add an element onto a non array value."

and eval_binary_merge ~state left_expression right_expression =
  let left = left_expression |> eval_expression ~state |> State.get_output in
  let right = right_expression |> eval_expression ~state |> State.get_output in
  let eval_merge left right =
    match (left.value_desc, right.value_desc) with
    | Array l, Array r ->
        Value.of_array
          ~value_loc:(Location.merge ~s:left.value_loc ~e:right.value_loc ())
          (Array.append l r)
    | Record l, Record r ->
        Value.of_string_map
          ~value_loc:(Location.merge ~s:left.value_loc ~e:right.value_loc ())
          (StringMap.union (fun _key _x y -> Some y) l r
          |> StringMap.to_seq
          |> Seq.mapi (fun index (key, (_, value)) -> (key, (index, value)))
          |> StringMap.of_seq)
    | HtmlTemplateNode (tag, attributes, children, self_closing), Record right ->
        let attributes =
          StringMap.union (fun _key _x y -> Some y) attributes (StringMap.map snd right)
        in
        {
          left with
          value_desc = HtmlTemplateNode (tag, attributes, children, self_closing);
        }
    | HtmlTemplateNode _, _ ->
        Pinc_Diagnostics.error
          right_expression.expression_loc
          "Trying to merge a non record value onto tag attributes."
    | ComponentTemplateNode (fn, tag, attributes, _), Record right ->
        let attributes =
          StringMap.union (fun _key _x y -> Some y) attributes (StringMap.map snd right)
        in
        let result = fn attributes in
        { left with value_desc = ComponentTemplateNode (fn, tag, attributes, result) }
    | ComponentTemplateNode _, _ ->
        Pinc_Diagnostics.error
          right_expression.expression_loc
          "Trying to merge a non record value onto component attributes."
    | Array _, _ ->
        Pinc_Diagnostics.error
          right_expression.expression_loc
          "Trying to merge a non array value onto an array."
    | _, Array _ ->
        Pinc_Diagnostics.error
          right_expression.expression_loc
          "Trying to merge an array value onto a non array."
    | _ ->
        Pinc_Diagnostics.error
          (Location.make
             ~s:left_expression.expression_loc.loc_start
             ~e:right_expression.expression_loc.loc_end
             ())
          "Trying to merge two non array values."
  in
  state |> State.add_output ~output:(eval_merge left right)

and eval_unary_not ~state expression =
  let expression_value = eval_expression ~state expression |> State.get_output in
  match expression_value.value_desc with
  | DefinitionInfo (name, typ, negated) ->
      let negated =
        match negated with
        | `Negated -> `NotNegated
        | `NotNegated -> `Negated
      in
      state
      |> State.add_output
           ~output:
             { expression_value with value_desc = DefinitionInfo (name, typ, negated) }
  | _ ->
      state
      |> State.add_output
           ~output:
             (Value.of_bool
                ~value_loc:expression.expression_loc
                (not (Value.is_true expression_value)))

and eval_unary_minus ~state expression =
  let expression_value = eval_expression ~state expression |> State.get_output in
  match expression_value.value_desc with
  | Int i ->
      state
      |> State.add_output
           ~output:(Value.of_int ~value_loc:expression.expression_loc (Int.neg i))
  | Float f ->
      state
      |> State.add_output
           ~output:(Value.of_float ~value_loc:expression.expression_loc (Float.neg f))
  | _ ->
      Pinc_Diagnostics.error
        expression.expression_loc
        "Invalid usage of unary `-` operator. You are only able to negate integers or \
         floats."

and eval_lowercase_identifier ~state ~loc ident =
  state |> State.get_value_from_scope ~ident |> function
  | None -> Pinc_Diagnostics.error loc ("Unbound identifier `" ^ ident ^ "`")
  | Some { value; is_mutable = _; is_optional = _ } ->
      state |> State.add_output ~output:value

and eval_let ~state ~ident ~is_mutable ~is_optional expression =
  let ident, ident_location = ident in
  let state =
    expression
    |> eval_expression
         ~state:{ state with binding_identifier = Some (is_optional, ident) }
  in
  let value = State.get_output state in
  match value with
  | { value_desc = Null; _ } when not is_optional ->
      Pinc_Diagnostics.error
        ident_location
        ("identifier " ^ ident ^ " is not marked as nullable, but was given a null value.")
  | value ->
      state
      |> State.add_value_to_scope ~ident ~value ~is_mutable ~is_optional
      |> State.add_output ~output:(Value.null ~value_loc:expression.expression_loc ())

and eval_use ~state ~ident expression =
  let ident, _ident_location = ident in
  let value = expression |> eval_expression ~state |> State.get_output in
  match value with
  | value ->
      state
      |> State.add_value_to_use_scope ~ident ~value
      |> State.add_output ~output:(Value.null ~value_loc:expression.expression_loc ())

and eval_mutation ~state ~ident expression =
  let ident, ident_location = ident in
  let current_binding = State.get_value_from_scope ~ident state in
  match current_binding with
  | None ->
      Pinc_Diagnostics.error
        ident_location
        "Trying to update a variable, which does not exist in the current scope."
  | Some { is_mutable = false; _ } ->
      Pinc_Diagnostics.error ident_location "Trying to update a non mutable variable."
  | Some { value = _; is_mutable = true; is_optional } ->
      let output =
        expression
        |> eval_expression
             ~state:{ state with binding_identifier = Some (is_optional, ident) }
      in
      let () =
        output |> State.get_output |> function
        | { value_desc = Null; _ } when not is_optional ->
            Pinc_Diagnostics.error
              ident_location
              ("identifier "
              ^ ident
              ^ " is not marked as nullable, but was tried to be updated with a null \
                 value.")
        | value -> state |> State.update_value_in_scope ~ident ~value
      in
      state
      |> State.add_output ~output:(Value.null ~value_loc:expression.expression_loc ())

and eval_if ~state ~condition ~alternate ~consequent =
  let condition_matches =
    condition |> eval_expression ~state |> State.get_output |> Value.is_true
  in
  match (condition_matches, alternate) with
  | true, _ -> consequent |> eval_statement ~state
  | false, Some alt -> alt |> eval_statement ~state
  | false, None ->
      state
      |> State.add_output
           ~output:
             (Value.null
                ~value_loc:
                  (Location.merge
                     ~s:condition.expression_loc
                     ~e:consequent.statement_loc
                     ())
                ())

and eval_for_in ~state ~index_ident ~ident ~reverse ~iterable body =
  let ident, _ident_location = ident in
  let iterable_value = iterable |> eval_expression ~state |> State.get_output in
  let index = ref (-1) in
  let rec loop ~state acc curr =
    match curr () with
    | Seq.Nil -> (state, List.rev acc)
    | Seq.Cons (value, tl) -> (
        index := succ !index;
        let state =
          state
          |> State.add_value_to_scope ~ident ~value ~is_mutable:false ~is_optional:false
        in
        let state =
          match index_ident with
          | Some (Lowercase_Id (ident, ident_location)) ->
              state
              |> State.add_value_to_scope
                   ~ident
                   ~value:(Value.of_int ~value_loc:ident_location !index)
                   ~is_mutable:false
                   ~is_optional:false
          | None -> state
        in
        match eval_statement ~state body with
        | exception Loop_Continue state -> loop ~state acc tl
        | exception Loop_Break state -> (state, List.rev acc)
        | state -> loop ~state (State.get_output state :: acc) tl)
  in
  match iterable_value.value_desc with
  | Array l ->
      let to_seq array =
        if reverse then (
          array |> Array.stable_sort (fun _ _ -> 1);
          array |> Array.to_seq)
        else
          Array.to_seq array
      in
      let state, res = l |> to_seq |> loop ~state [] in
      state
      |> State.add_output ~output:(res |> Value.of_list ~value_loc:body.statement_loc)
  | String s ->
      let map s =
        if reverse then
          s
          |> CCUtf8_string.to_seq
          |> CCSeq.to_rev_list
          |> List.to_seq
          |> Seq.map (fun c -> c |> Value.of_char ~value_loc:iterable_value.value_loc)
        else
          s
          |> CCUtf8_string.to_seq
          |> Seq.map (fun c -> c |> Value.of_char ~value_loc:iterable_value.value_loc)
      in
      let state, res = s |> CCUtf8_string.of_string_exn |> map |> loop ~state [] in
      state
      |> State.add_output ~output:(res |> Value.of_list ~value_loc:body.statement_loc)
  | Null -> state |> State.add_output ~output:iterable_value
  | HtmlTemplateNode _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over template node"
  | ComponentTemplateNode _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over template node"
  | Portal _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over portal value"
  | Record _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over record value"
  | Int _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over int value"
  | Char _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over char value"
  | Float _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over float value"
  | Bool _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over boolean value"
  | DefinitionInfo _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over definition info"
  | Function _ ->
      Pinc_Diagnostics.error
        iterable.expression_loc
        "Cannot iterate over function definition"
  | TagInfo _ -> assert false

and eval_range ~state ~inclusive from_expression upto_expression =
  let from = from_expression |> eval_expression ~state |> State.get_output in
  let upto = upto_expression |> eval_expression ~state |> State.get_output in
  let get_range from upto =
    match (from.value_desc, upto.value_desc) with
    | Int from, Int upto -> (from, upto)
    | Int _, _ ->
        Pinc_Diagnostics.error
          upto.value_loc
          "Can't construct range in for loop. The end of your range is not of type int."
    | _, Int _ ->
        Pinc_Diagnostics.error
          from.value_loc
          "Can't construct range in for loop. The start of your range is not of type int."
    | _, _ ->
        Pinc_Diagnostics.error
          (Location.merge ~s:from.value_loc ~e:upto.value_loc ())
          "Can't construct range in for loop. The start and end of your range are not of \
           type int."
  in
  let from_int, upto_int = get_range from upto in
  let iter =
    if from_int > upto_int then
      [||]
    else (
      let start = from_int in
      let stop =
        if inclusive then
          upto_int + 1
        else
          upto_int
      in
      Array.init (stop - start) (fun i ->
          Value.of_int
            ~value_loc:(Location.merge ~s:from.value_loc ~e:upto.value_loc ())
            (i + start)))
  in
  state
  |> State.add_output
       ~output:
         (Value.of_array
            ~value_loc:(Location.merge ~s:from.value_loc ~e:upto.value_loc ())
            iter)

and eval_block ~state statements =
  let state = state |> State.add_scope in
  statements |> List.fold_left (fun state -> eval_statement ~state) state

and eval_slot ~tag ~attributes ~slotted_elements key =
  let find_slot_key attributes =
    attributes
    |> StringMap.find_opt "slot"
    |> Option.value ~default:(Value.of_string ~value_loc:tag.Ast.tag_loc "")
    |> function
    | { value_desc = String s; _ } -> s
    | { value_loc; _ } ->
        Pinc_Diagnostics.error value_loc "Expected slot attribute to be of type string"
  in
  let rec keep_slotted acc el =
    match el with
    | ( { value_desc = HtmlTemplateNode (_, attributes, _, _); _ }
      | { value_desc = ComponentTemplateNode (_, _, attributes, _); _ } ) as v ->
        if find_slot_key attributes = key then
          v :: acc
        else
          acc
    | { value_desc = Array l; _ } -> l |> Array.fold_left keep_slotted acc
    | { value_desc = String s; _ } when String.trim s = "" -> acc
    | { value_loc; _ } ->
        Pinc_Diagnostics.error
          value_loc
          "Only template nodes are allowed inside slots. If you want to put another \
           value (like a string) into a slot, you have to wrap it in some html tag or \
           component."
  in
  let slotted_elements = slotted_elements |> List.fold_left keep_slotted [] |> List.rev in
  let min =
    attributes
    |> StringMap.find_opt "min"
    |> Option.map (function
           | { value_desc = Int i; _ } -> i
           | { value_loc; _ } ->
               Pinc_Diagnostics.error
                 value_loc
                 "Expected attribute min to be of tyoe int.")
    |> Option.value ~default:0
  in
  let max =
    attributes
    |> StringMap.find_opt "max"
    |> Option.map (function
           | { value_desc = Int i; _ } -> i
           | { value_loc; _ } ->
               Pinc_Diagnostics.error
                 value_loc
                 "Expected attribute max to be of tyoe int.")
    |> Option.value ~default:Int.max_int
  in
  let num_slotted_elements = List.length slotted_elements in
  let () =
    match (num_slotted_elements < min, num_slotted_elements > max) with
    | true, _ ->
        Pinc_Diagnostics.error
          tag.Ast.tag_loc
          (Printf.sprintf
             "This #Slot did not reach the minimum amount of nodes (specified as %i)."
             min)
    | _, true ->
        Pinc_Diagnostics.error
          tag.Ast.tag_loc
          (Printf.sprintf
             "This #Slot was provided more than the maximum amount of nodes (specified \
              as %i)."
             max)
    | false, false -> ()
  in
  let constraints =
    attributes
    |> StringMap.find_opt "constraints"
    |> Option.map (function
           | { value_desc = Array a; _ } -> a
           | { value_desc = _; value_loc } ->
               Pinc_Diagnostics.error
                 value_loc
                 "slot contraints need to be an array of definitions which are either \
                  allowed or disallowed")
    |> Option.map Array.to_list
    |> Option.map
         (List.map (function
             | { value_desc = DefinitionInfo (name, Some `Component, negated); _ } ->
                 (`Component, name, negated)
             | { value_desc = DefinitionInfo (name, None, _negated); value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   ("definition `" ^ name ^ "` does not exist")
             | { value_desc = DefinitionInfo (name, _typ, _negated); value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   ("definition `"
                   ^ name
                   ^ "` is not a component. Expected to see a component definition at \
                      this point.")
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   "Expected to see a component definition at this point"))
  in
  let check_instance_restriction tag =
    match constraints with
    | None -> Result.ok ()
    | Some [] ->
        Result.error
          (Printf.sprintf
             "Child with tag `%s` may not be used inside this #Slot. \n\
              It has an empty array set as constrints, which leads to nothing being \
              allowed to be placed inside."
             tag)
    | Some restrictions ->
        let is_in_list = ref false in
        let allowed, disallowed =
          restrictions
          |> List.partition_map (fun (_typ, name, negated) ->
                 if name = tag then
                   is_in_list := true;
                 if negated = `Negated then
                   Either.right name
                 else
                   Either.left name)
        in
        let is_allowed =
          match (allowed, disallowed) with
          | [], _disallowed -> not !is_in_list
          | _allowed, [] -> !is_in_list
          | allowed, _disallowed -> List.mem tag allowed
        in
        if is_allowed then
          Result.ok ()
        else (
          let contraints =
            constraints
            |> Option.value ~default:[]
            |> List.map (fun (_typ, name, negated) ->
                   if negated = `Negated then
                     "!" ^ name
                   else
                     name)
            |> String.concat ","
          in
          Result.error
            (Printf.sprintf
               "Child with tag `%s` may not be used inside this #Slot. The following \
                restrictions are set: [ %s ]"
               tag
               contraints))
  in
  slotted_elements
  |> List.map (function
         | ( { value_desc = HtmlTemplateNode (tag_name, _, _, _); value_loc }
           | { value_desc = ComponentTemplateNode (_, tag_name, _, _); value_loc } ) as v
           -> (
             match check_instance_restriction tag_name with
             | Ok () -> v
             | Error e -> Pinc_Diagnostics.error value_loc e)
         | { value_desc = _; value_loc } ->
             Pinc_Diagnostics.error
               value_loc
               "Tried to assign a non node value to a #Slot. Only template nodes are \
                allowed inside slots. If you want to put another value (like a string) \
                into a slot, you have to wrap it in some html tag or component.")
  |> Value.of_list ~value_loc:tag.Ast.tag_loc

and eval_internal_tag ~state ~tag ~key ~attributes ~value_bag tag_identifier =
  match tag_identifier with
  | `String ->
      value_bag
      |> StringMap.find_opt key
      |> Option.map (function
             | { value_desc = String _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf "Expected attribute %s to be of type string." key))
      |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())
  | `Int ->
      value_bag
      |> StringMap.find_opt key
      |> Option.map (function
             | { value_desc = Int _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf "Expected attribute %s to be of type int." key))
      |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())
  | `Float ->
      value_bag
      |> StringMap.find_opt key
      |> Option.map (function
             | { value_desc = Float _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf "Expected attribute %s to be of type float." key))
      |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())
  | `Boolean ->
      value_bag
      |> StringMap.find_opt key
      |> Option.map (function
             | { value_desc = Bool _; _ } as value -> value
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf "Expected attribute %s to be of type bool." key))
      |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())
  | `Array ->
      let of' =
        attributes |> StringMap.find_opt "of" |> function
        | Some { value_desc = TagInfo i; _ } -> i
        | Some { value_desc = _; value_loc } ->
            Pinc_Diagnostics.error
              value_loc
              "Attribute `of` needs to be a tag value describing the type of values in \
               this array."
        | None ->
            Pinc_Diagnostics.error tag.Ast.tag_loc "Attribute `of` is required on #Array."
      in
      value_bag
      |> StringMap.find_opt key
      |> Option.map (function
             | { value_desc = Array a; _ } -> a
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf "Expected attribute %s to be an array." key))
      |> Option.map (fun array ->
             array
             |> Array.mapi (fun index item ->
                    let key = string_of_int index in
                    { of' with key }
                    |> eval_internal_or_external_tag
                         ~state
                         ~tag
                         ~value:(StringMap.singleton key item))
             |> Value.of_array ~value_loc:tag.Ast.tag_loc)
      |> Option.value ~default:(Value.null ~value_loc:tag.tag_loc ())
  | `Record ->
      let of' =
        attributes |> StringMap.find_opt "of" |> function
        | Some { value_desc = Record r; _ } -> r
        | Some { value_desc = _; value_loc } ->
            Pinc_Diagnostics.error
              value_loc
              "Attribute `of` needs to be a record describing the shape and type of \
               values in this record."
        | None ->
            Pinc_Diagnostics.error
              tag.Ast.tag_loc
              "Attribute `of` is required on #Record."
      in
      value_bag
      |> StringMap.find_opt key
      |> Option.map (function
             | { value_desc = Record r; _ } -> StringMap.map snd r
             | { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   (Printf.sprintf "Expected attribute %s to be a record." key))
      |> Option.map (fun record ->
             StringMap.merge
               (fun _key x y ->
                 match (x, y) with
                 | Some _, Some (idx, { value_desc = TagInfo info; _ }) ->
                     Some
                       (idx, eval_internal_or_external_tag ~state ~tag ~value:record info)
                 | None, Some (idx, { value_desc = TagInfo _; value_loc }) ->
                     Some (idx, Value.null ~value_loc ())
                 | (None | Some _), Some (idx, value) -> Some (idx, value)
                 | (None | Some _), None -> None)
               record
               of'
             |> Value.of_string_map ~value_loc:tag.Ast.tag_loc)
      |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())

and eval_internal_or_external_tag ~state ~tag ?value tag_info =
  let { tag = tag_identifier; key; required = _; attributes; transformer } = tag_info in
  match (value, state.parent_component) with
  | None, Some (_, value_bag, slotted_elements)
  | Some value_bag, Some (_, _, slotted_elements) -> (
      match tag_identifier with
      | (`Array | `Boolean | `Float | `Int | `Record | `String) as tag_identifier ->
          tag_identifier
          |> eval_internal_tag ~state ~tag ~key ~attributes ~value_bag
          |> transformer
      | `Slot -> key |> eval_slot ~tag ~attributes ~slotted_elements |> transformer
      | `Custom _ -> tag_info |> call_tag_listener ~state ~tag ~value_bag:(Some value_bag)
      )
  | value_bag, _ -> tag_info |> call_tag_listener ~state ~tag ~value_bag

and call_tag_listener ~state ~tag ~value_bag t =
  let { tag = tag_identifier; key; required; attributes; transformer } = t in
  let listener = Hashtbl.find_opt state.tag_listeners tag_identifier in
  (match listener with
  | Some (`String fn) -> fn ~required ~attributes ~key
  | Some (`Int fn) -> fn ~required ~attributes ~key
  | Some (`Float fn) -> fn ~required ~attributes ~key
  | Some (`Boolean fn) -> fn ~required ~attributes ~key
  | Some (`Array fn) ->
      let child =
        attributes |> StringMap.find_opt "of" |> function
        | Some { value_desc = TagInfo i; _ } -> i
        | Some { value_desc = _; value_loc } ->
            Pinc_Diagnostics.error
              value_loc
              "Attribute `of` needs to be a tag value describing the type of values in \
               this array."
        | None ->
            Pinc_Diagnostics.error tag.Ast.tag_loc "Attribute `of` is required on #Array."
      in
      fn ~required ~attributes ~child ~key
  | Some (`Record fn) ->
      let children =
        attributes
        |> StringMap.find_opt "of"
        |> (function
             | Some { value_desc = Record r; _ } -> r
             | Some { value_desc = _; value_loc } ->
                 Pinc_Diagnostics.error
                   value_loc
                   "Attribute `of` needs to be a record describing the shape and type of \
                    values in this record."
             | None ->
                 Pinc_Diagnostics.error
                   tag.Ast.tag_loc
                   "Attribute `of` is required on #Record.")
        |> StringMap.filter_map (fun _key (index, value) ->
               match value with
               | { value_desc = TagInfo i; _ } -> Some (index, i)
               | _ -> None)
        |> StringMap.to_seq
        |> List.of_seq
        |> List.fast_sort (fun (_key, (index_a, _value)) (_key, (index_b, _value)) ->
               index_a - index_b)
        |> List.map (fun (key, (_index, value)) -> (key, value))
      in
      fn ~required ~attributes ~children ~key
  | Some (`Slot fn) -> fn ~required ~attributes ~key
  | Some (`Custom fn) -> fn ~required ~attributes ~parent_value:value_bag ~key
  | None -> Result.ok (Value.null ~value_loc:tag.tag_loc ()))
  |> function
  | Ok v -> v |> transformer
  | Error e -> Pinc_Diagnostics.error tag.tag_loc e

and eval_tag ~state tag =
  let Ast.{ tag = tag_identifier; attributes; transformer } = tag.tag_desc in
  let required =
    not (state.binding_identifier |> Option.map fst |> Option.value ~default:false)
  in
  let of' = Option.bind attributes (fun attrs -> attrs |> StringMap.find_opt "of") in
  let tag_attributes =
    match attributes with
    | Some attributes ->
        attributes
        |> StringMap.remove "of"
        |> StringMap.map (fun it -> it |> eval_expression ~state |> State.get_output)
    | None -> StringMap.empty
  in
  let key, state =
    match (StringMap.find_opt "key" tag_attributes, state.binding_identifier) with
    | None, Some (_optional, ident) -> (ident, { state with binding_identifier = None })
    | Some { value_desc = String key; _ }, _ -> (key, state)
    | Some { value_desc = _; value_loc }, _ ->
        Pinc_Diagnostics.error
          value_loc
          "Expected attribute `key` on tag to be of type string"
    | None, None -> ("", state)
  in
  let path = (state.parent_tag |> Option.value ~default:[]) @ [ key ] in
  let tag_attributes =
    tag_attributes
    |> StringMap.remove "key"
    |> StringMap.add
         "of"
         (of'
         |> Option.map (fun it ->
                it
                |> eval_expression ~state:{ state with parent_tag = Some path }
                |> State.get_output)
         |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ()))
  in
  let apply_transformer ~transformer value =
    match transformer with
    | Some transformer ->
        let Ast.Lowercase_Id (ident, _ident_location), expr = transformer in
        let state =
          state
          |> State.add_scope
          |> State.add_value_to_scope ~ident ~value ~is_optional:false ~is_mutable:false
        in
        eval_expression ~state expr |> State.get_output
    | _ -> value
  in
  let value =
    match (state.mode, tag_identifier) with
    | _, `CreatePortal ->
        {
          value_desc = Portal (Hashtbl.find_all state.portals key);
          value_loc = tag.Ast.tag_loc;
        }
    | _, `SetContext ->
        let value =
          tag_attributes |> StringMap.find_opt "value" |> function
          | None ->
              Pinc_Diagnostics.error
                tag.tag_loc
                "attribute value is required when setting a context."
          | Some { value_desc = Function _; value_loc } ->
              Pinc_Diagnostics.error value_loc "a function can not be put into a context."
          | Some value -> value
        in
        Hashtbl.add state.context key value;
        Value.null ~value_loc:tag.Ast.tag_loc ()
    | _, `GetContext ->
        Hashtbl.find_opt state.context key
        |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())
    | Portal_Collection, `Portal ->
        let push =
          match tag_attributes |> StringMap.find_opt "push" with
          | None ->
              Pinc_Diagnostics.error
                tag.tag_loc
                "The attribute `push` is required when pushing a value into a portal."
          | Some { value_desc = Function _; value_loc } ->
              Pinc_Diagnostics.error value_loc "A function can not be put into a portal."
          | Some value -> value
        in
        Hashtbl.add state.portals key push;
        Value.null ~value_loc:tag.Ast.tag_loc ()
    | Render, `Portal -> Value.null ~value_loc:tag.Ast.tag_loc ()
    | ( Portal_Collection,
        ((`Array | `Boolean | `Custom _ | `Float | `Int | `Record | `Slot | `String) as
        tag_identifier) ) -> (
        let tag_info =
          {
            tag = tag_identifier;
            key;
            required;
            attributes = tag_attributes;
            transformer = apply_transformer ~transformer;
          }
        in
        match state.parent_tag with
        | None -> tag_info |> eval_internal_or_external_tag ~tag ~state
        | Some _ -> { value_desc = TagInfo tag_info; value_loc = tag.Ast.tag_loc })
    | Render, _ ->
        Option.bind (Hashtbl.find_opt state.tag_cache key) Queue.take_opt
        |> Option.value ~default:(Value.null ~value_loc:tag.Ast.tag_loc ())
  in
  (* TODO: Cleanup! Ideally the tag_cache would be a Hashtable of unique keys per tag! *)
  if state.mode = Portal_Collection then (
    match Hashtbl.find_opt state.tag_cache key with
    | None ->
        let q = Queue.create () in
        Queue.add value q;
        Hashtbl.add state.tag_cache key q
    | Some q -> Queue.add value q);
  state |> State.add_output ~output:value

and eval_template ~state template =
  match template.template_node_desc with
  | Ast.TextTemplateNode text ->
      state
      |> State.add_output
           ~output:(Value.of_string ~value_loc:template.template_node_loc text)
  | Ast.HtmlTemplateNode
      {
        html_tag_identifier;
        html_tag_attributes;
        html_tag_children;
        html_tag_self_closing;
      } ->
      let html_tag_attributes =
        html_tag_attributes
        |> StringMap.map (eval_expression ~state)
        |> StringMap.map State.get_output
      in
      let html_tag_children =
        html_tag_children |> List.map (eval_template ~state) |> List.map State.get_output
      in
      state
      |> State.add_output
           ~output:
             {
               value_loc = template.template_node_loc;
               value_desc =
                 HtmlTemplateNode
                   ( html_tag_identifier,
                     html_tag_attributes,
                     html_tag_children,
                     html_tag_self_closing );
             }
  | Ast.ExpressionTemplateNode expr -> eval_expression ~state expr
  | Ast.ComponentTemplateNode
      {
        component_tag_identifier = Uppercase_Id (component_tag_identifier, _);
        component_tag_attributes;
        component_tag_children;
      } ->
      let component_tag_attributes =
        component_tag_attributes
        |> StringMap.map (eval_expression ~state)
        |> StringMap.map State.get_output
      in
      let component_tag_children =
        component_tag_children
        |> List.map (eval_template ~state)
        |> List.map State.get_output
      in
      let render_fn component_tag_attributes =
        let state =
          State.make
            ~parent_component:
              (component_tag_identifier, component_tag_attributes, component_tag_children)
            ~tag_listeners:state.tag_listeners
            ~context:state.context
            ~portals:state.portals
            ~tag_cache:state.tag_cache
            ~mode:state.mode
            state.declarations
        in
        eval_declaration ~state component_tag_identifier |> State.get_output
      in
      let result = render_fn component_tag_attributes in
      state
      |> State.add_output
           ~output:
             {
               value_loc = template.template_node_loc;
               value_desc =
                 ComponentTemplateNode
                   (render_fn, component_tag_identifier, component_tag_attributes, result);
             }

and eval_declaration ~state declaration =
  state.declarations |> StringMap.find_opt declaration |> function
  | Some Ast.{ declaration_type = Declaration_Component { declaration_body; _ }; _ }
  | Some Ast.{ declaration_type = Declaration_Library { declaration_body; _ }; _ }
  | Some Ast.{ declaration_type = Declaration_Site { declaration_body; _ }; _ }
  | Some Ast.{ declaration_type = Declaration_Page { declaration_body; _ }; _ }
  | Some Ast.{ declaration_type = Declaration_Store { declaration_body; _ }; _ } ->
      eval_expression ~state declaration_body
  | None ->
      Pinc_Diagnostics.error
        Location.none
        ("Declaration with name `" ^ declaration ^ "` was not found.")
;;

let eval_meta ?tag_listeners declarations =
  let state = State.make ?tag_listeners declarations ~mode:Render in
  let eval attrs =
    attrs
    |> Option.value ~default:StringMap.empty
    |> StringMap.map (fun e -> eval_expression ~state e |> State.get_output)
  in
  let open Ast in
  declarations
  |> StringMap.map (function
         | { declaration_type = Declaration_Component { declaration_attributes; _ }; _ }
           -> `Component (eval declaration_attributes)
         | { declaration_type = Declaration_Library { declaration_attributes; _ }; _ } ->
             `Library (eval declaration_attributes)
         | { declaration_type = Declaration_Site { declaration_attributes; _ }; _ } ->
             `Site (eval declaration_attributes)
         | { declaration_type = Declaration_Page { declaration_attributes; _ }; _ } ->
             `Page (eval declaration_attributes)
         | { declaration_type = Declaration_Store { declaration_attributes; _ }; _ } ->
             `Store (eval declaration_attributes))
;;

let eval ?tag_listeners ~root declarations =
  let state = State.make ?tag_listeners declarations ~mode:Portal_Collection in
  let state = eval_declaration ~state root in
  if state.portals |> Hashtbl.length > 0 then
    eval_declaration ~state:{ state with mode = Render } root
  else
    state
;;

let from_source ?(filename = "") ~source root =
  let declarations = Parser.parse ~filename source in
  eval ~root declarations
;;
