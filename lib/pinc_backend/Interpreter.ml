open Types
module Types = Types
module Ast = Pinc_Parser.Ast
module Parser = Pinc_Parser
module Location = Pinc_Diagnostics.Location
module Source = Pinc_Source

exception Loop_Break of state
exception Loop_Continue of state

let libraries = Hashtbl.create 100
let add_library ~ident ~library = Hashtbl.add libraries ident library
let stores = Hashtbl.create 100
let add_store ~ident ~store = Hashtbl.add stores ident store

let rec get_uppercase_identifier_typ ~state ident =
  let declaration = state.declarations |> StringMap.find_opt ident in
  match declaration with
  | None -> (state, None)
  | Some { declaration_type = Ast.Declaration_Component _; _ } ->
      (state, Some Definition_Component)
  | Some { declaration_type = Ast.Declaration_Page _; _ } -> (state, Some Definition_Page)
  | Some
      {
        declaration_type =
          Ast.Declaration_Store { declaration_attributes; declaration_body };
        _;
      } -> (
      match Hashtbl.find_opt stores ident with
      | Some store -> (state, Some (Definition_Store store))
      | None ->
          let singleton =
            declaration_attributes
            |> StringMap.find_opt "single"
            |> Option.map (fun expr ->
                   expr
                   |> eval_expression
                        ~state:
                          (State.make
                             ~root_tag_data_provider:state.root_tag_data_provider
                             ~tag_data_provider:state.tag_data_provider
                             ~root_tag_meta_provider:state.root_tag_meta_provider
                             ~tag_meta_provider:state.tag_meta_provider
                             ~tag_meta:state.tag_meta
                             ~mode:state.mode
                             state.declarations)
                   |> State.get_output
                   |> function
                   | { value_desc = Bool b; _ } -> b
                   | { value_loc; _ } ->
                       Pinc_Diagnostics.error
                         value_loc
                         "The attribute `single` has to be a boolean.")
            |> Option.value ~default:false
          in
          let store = Type_Store.make ~singleton ~body:declaration_body in
          add_store ~store ~ident;
          (state, Some (Definition_Store store)))
  | Some { declaration_type = Ast.Declaration_Library { declaration_body; _ }; _ } -> (
      match Hashtbl.find_opt libraries ident with
      | Some library -> (state, Some (Definition_Library library))
      | None ->
          let s =
            eval_expression
              ~state:{ state with environment = { state.environment with scope = [] } }
              declaration_body
          in
          let bindings = s |> State.get_bindings in
          let includes = s |> State.get_used_values in
          let library = Type_Library.make ~bindings ~includes in
          add_library ~library ~ident;
          (state, Some (Definition_Library library)))

and eval_statement ~state statement =
  let result =
    match statement.Ast.statement_desc with
    | Ast.CommentStatement _ -> state
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
    | Ast.UseStatement (ident, expression) -> eval_use ~state ~ident expression
    | Ast.BreakStatement _ -> raise_notrace (Loop_Break state)
    | Ast.ContinueStatement _ -> raise_notrace (Loop_Continue state)
    | Ast.ExpressionStatement expression -> expression |> eval_expression ~state
  in
  { result with binding_identifier = None }

and eval_expression ~state expression =
  match expression.expression_desc with
  | Ast.Comment _ -> state
  | Ast.Char c ->
      state
      |> State.add_output ~output:(Helpers.Value.char ~loc:expression.expression_loc c)
  | Ast.Int i ->
      state
      |> State.add_output ~output:(Helpers.Value.int ~loc:expression.expression_loc i)
  | Ast.Float f when Float.is_integer f ->
      state
      |> State.add_output
           ~output:(Helpers.Value.int ~loc:expression.expression_loc (int_of_float f))
  | Ast.Float f ->
      state
      |> State.add_output ~output:(Helpers.Value.float ~loc:expression.expression_loc f)
  | Ast.Bool b ->
      state
      |> State.add_output ~output:(Helpers.Value.bool ~loc:expression.expression_loc b)
  | Ast.Array l ->
      let output =
        l
        |> Array.map (fun it -> it |> eval_expression ~state |> State.get_output)
        |> Helpers.Value.array ~loc:expression.expression_loc
      in
      state |> State.add_output ~output
  | Ast.Record map ->
      let state, seq =
        map
        |> StringMap.to_seq
        |> Seq.fold_left
             (fun (state, seq) (ident, (requirement, expression)) ->
               let state =
                 expression
                 |> eval_expression
                      ~state:{ state with binding_identifier = Some (requirement, ident) }
               in
               let output = state |> State.get_output in
               let () =
                 match output with
                 | { value_desc = Null; value_loc } when requirement = `Required ->
                     Pinc_Diagnostics.error
                       value_loc
                       (Printf.sprintf
                          "identifier %s is not marked as nullable, but was given a null \
                           value."
                          ident)
                 | _value -> ()
               in
               (state, seq |> Seq.cons (ident, output)))
             (state, Seq.empty)
      in
      let output =
        seq |> StringMap.of_seq |> Helpers.Value.record ~loc:expression.expression_loc
      in
      state |> State.add_output ~output
  | Ast.String template -> eval_string_template ~state template
  | Ast.Function { parameters; body } ->
      eval_function_declaration ~loc:expression.expression_loc ~state ~parameters body
  | Ast.FunctionCall { function_definition; arguments } ->
      eval_function_call ~state ~arguments function_definition
  | Ast.UppercaseIdentifierPathExpression path -> (
      let rec eval_library_path ~state (name, library) path =
        match path with
        | [] -> (state, name, library)
        | hd :: tl -> (
            match library |> Type_Library.get_include hd with
            | Some l -> eval_library_path ~state (hd, l) tl
            | None ->
                Pinc_Diagnostics.error
                  expression.expression_loc
                  (Printf.sprintf
                     "Library with name `%s` could not be found inside `%s`."
                     hd
                     name))
      in
      match path with
      | [] -> assert false
      | hd :: tl -> (
          let state, library =
            state
            |> State.get_used_values
            |> StringMap.find_opt hd
            |> Option.fold
                 ~some:(fun l -> (state, Some (Definition_Library l)))
                 ~none:(get_uppercase_identifier_typ ~state hd)
          in
          match library with
          | Some (Definition_Library l) ->
              let state, name, library = eval_library_path ~state (hd, l) tl in
              let output =
                {
                  value_loc = expression.expression_loc;
                  value_desc =
                    DefinitionInfo (name, Some (Definition_Library library), `NotNegated);
                }
              in
              state |> State.add_output ~output
          | Some _ ->
              Pinc_Diagnostics.error
                expression.expression_loc
                (Printf.sprintf
                   "`%s` is not a library. Cannot construct a path with non library \
                    definitions."
                   hd)
          | None ->
              Pinc_Diagnostics.error
                expression.expression_loc
                (Printf.sprintf "Library with name `%s` could not be found." hd)))
  | Ast.UppercaseIdentifierExpression id ->
      let state, typ =
        state
        |> State.get_used_values
        |> StringMap.find_opt id
        |> Option.fold
             ~some:(fun l -> (state, Some (Definition_Library l)))
             ~none:(get_uppercase_identifier_typ ~state id)
      in
      let output =
        {
          value_desc = DefinitionInfo (id, typ, `NotNegated);
          value_loc = expression.expression_loc;
        }
      in
      state |> State.add_output ~output
  | Ast.LowercaseIdentifierExpression id ->
      eval_lowercase_identifier ~loc:expression.expression_loc ~state id
  | Ast.TagExpression tag -> Tag.eval ~eval_expression ~state tag
  | Ast.ForInExpression { index; iterator = Lowercase_Id ident; reverse; iterable; body }
    -> eval_for_in ~state ~index_ident:index ~ident ~reverse ~iterable body
  | Ast.TemplateExpression nodes ->
      state
      |> State.add_output
           ~output:
             (nodes
             |> List.map (fun it -> it |> eval_template ~state |> State.get_output)
             |> Helpers.Value.list ~loc:expression.expression_loc)
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
                | StringInterpolation (Lowercase_Id (id, loc)) ->
                    id
                    |> eval_lowercase_identifier ~loc ~state
                    |> State.get_output
                    |> Value.to_string)
         |> String.concat ""
         |> Helpers.Value.string ~loc:(Location.merge ~s:!start_loc ~e:!end_loc ()))

and eval_function_declaration ~state ~loc ~parameters body =
  let ident = state.binding_identifier in
  let rec exec ~arguments ~state () =
    let state = state |> State.add_scope in
    let state =
      match ident with
      | None -> state
      | Some (_, ident) ->
          state
          |> State.add_value_to_scope
               ~ident
               ~value:fn
               ~is_mutable:false
               ~is_optional:false
    in
    let state =
      state
      |> StringMap.fold
           (fun ident value ->
             State.add_value_to_scope ~ident ~value ~is_mutable:false ~is_optional:false)
           arguments
    in
    let state = eval_expression ~state body in
    let state = state |> State.remove_scope in
    state |> State.get_output
  and fn = { value_loc = loc; value_desc = Function { parameters; state; exec } } in

  ident
  |> Option.iter (fun (_, ident) ->
         state
         |> State.add_value_to_function_scopes
              ~ident
              ~value:fn
              ~is_optional:false
              ~is_mutable:false);

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
             (Helpers.Value.char
                ~loc:merged_value_loc
                Uchar.(of_int (to_int a + to_int b)))
  | Char a, Int b ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.char ~loc:merged_value_loc Uchar.(of_int (to_int a + b)))
  | Int a, Char b ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.char ~loc:merged_value_loc Uchar.(of_int (a + to_int b)))
  | Int a, Int b ->
      state |> State.add_output ~output:(Helpers.Value.int ~loc:merged_value_loc (a + b))
  | Float a, Float b ->
      state
      |> State.add_output ~output:(Helpers.Value.float ~loc:merged_value_loc (a +. b))
  | Float a, Int b ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:merged_value_loc (a +. float_of_int b))
  | Int a, Float b ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:merged_value_loc (float_of_int a +. b))
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
             (Helpers.Value.char
                ~loc:merged_value_loc
                Uchar.(of_int (to_int a - to_int b)))
  | Char a, Int b ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.char ~loc:merged_value_loc Uchar.(of_int (to_int a - b)))
  | Int a, Char b ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.char ~loc:merged_value_loc Uchar.(of_int (a - to_int b)))
  | Int a, Int b ->
      state |> State.add_output ~output:(Helpers.Value.int ~loc:merged_value_loc (a - b))
  | Float a, Float b ->
      state
      |> State.add_output ~output:(Helpers.Value.float ~loc:merged_value_loc (a -. b))
  | Float a, Int b ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:merged_value_loc (a -. float_of_int b))
  | Int a, Float b ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:merged_value_loc (float_of_int a -. b))
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
             (Helpers.Value.char
                ~loc:merged_value_loc
                Uchar.(of_int (to_int a * to_int b)))
  | Char a, Int b ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.char ~loc:merged_value_loc Uchar.(of_int (to_int a * b)))
  | Int a, Char b ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.char ~loc:merged_value_loc Uchar.(of_int (a * to_int b)))
  | Int a, Int b ->
      state |> State.add_output ~output:(Helpers.Value.int ~loc:merged_value_loc (a * b))
  | Float a, Float b ->
      state
      |> State.add_output ~output:(Helpers.Value.float ~loc:merged_value_loc (a *. b))
  | Float a, Int b ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:merged_value_loc (a *. float_of_int b))
  | Int a, Float b ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:merged_value_loc (float_of_int a *. b))
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
    | Int _, Int 0 | Float _, Float 0. | Float _, Int 0 | Int _, Float 0. -> 0.
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
    |> State.add_output ~output:(Helpers.Value.int ~loc:merged_value_loc (int_of_float r))
  else
    state |> State.add_output ~output:(Helpers.Value.float ~loc:merged_value_loc r)

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
    |> State.add_output ~output:(Helpers.Value.int ~loc:merged_value_loc (int_of_float r))
  else
    state |> State.add_output ~output:(Helpers.Value.float ~loc:merged_value_loc r)

and eval_binary_modulo ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let ( % ) = ( mod ) in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  let r =
    match (a.value_desc, b.value_desc) with
    | Int _, Int 0 | Int _, Float 0. | Float _, Float 0. | Float _, Int 0 -> 0.
    | Int a, Int b -> float_of_int (a % b)
    | Float a, Float b -> a -. (a /. b *. b)
    | Float a, Int 1 -> fst (Float.modf a)
    | Float a, Int b ->
        let b = float_of_int b in
        a -. (a /. b *. b)
    | Int a, Float b ->
        let a = float_of_int a in
        a -. (a /. b *. b)
    | (Int _ | Float _), _ ->
        Pinc_Diagnostics.error b.value_loc "Trying to modulo non numeric literals."
    | _, (Int _ | Float _) ->
        Pinc_Diagnostics.error a.value_loc "Trying to modulo non numeric literals."
    | _ ->
        Pinc_Diagnostics.error merged_value_loc "Trying to modulo non numeric literals."
  in
  state
  |> State.add_output
       ~output:
         (if Float.is_integer r then
            Helpers.Value.int ~loc:merged_value_loc (int_of_float r)
          else
            Helpers.Value.float ~loc:merged_value_loc r)

and eval_binary_and ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:
         (Helpers.Value.bool ~loc:merged_value_loc (Value.is_true a && Value.is_true b))

and eval_binary_or ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:
         (Helpers.Value.bool ~loc:merged_value_loc (Value.is_true a || Value.is_true b))

and eval_binary_less ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Helpers.Value.bool ~loc:merged_value_loc (Value.compare a b < 0))

and eval_binary_less_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Helpers.Value.bool ~loc:merged_value_loc (Value.compare a b <= 0))

and eval_binary_greater ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Helpers.Value.bool ~loc:merged_value_loc (Value.compare a b > 0))

and eval_binary_greater_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Helpers.Value.bool ~loc:merged_value_loc (Value.compare a b >= 0))

and eval_binary_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output ~output:(Helpers.Value.bool ~loc:merged_value_loc (Value.equal a b))

and eval_binary_not_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let merged_value_loc = Location.merge ~s:a.value_loc ~e:b.value_loc () in
  state
  |> State.add_output
       ~output:(Helpers.Value.bool ~loc:merged_value_loc (not (Value.equal a b)))

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
       ~output:(Helpers.Value.string ~loc:merged_value_loc (Buffer.contents buf))

and eval_binary_dot_access ~state left right =
  let state = left |> eval_expression ~state in
  let left_value = state |> State.get_output in
  match (left_value.value_desc, right.expression_desc) with
  | Null, _ ->
      state |> State.add_output ~output:(Helpers.Value.null ~loc:left_value.value_loc ())
  | Record a, Ast.LowercaseIdentifierExpression b ->
      let output =
        a
        |> StringMap.find_opt b
        |> Option.value ~default:(Helpers.Value.null ~loc:left.expression_loc ())
      in
      state |> State.add_output ~output
  | HtmlTemplateNode (tag, attributes, _, _), Ast.LowercaseIdentifierExpression b -> (
      match b with
      | "tag" ->
          state
          |> State.add_output ~output:(Helpers.Value.string ~loc:left.expression_loc tag)
      | "attributes" ->
          state
          |> State.add_output
               ~output:(Helpers.Value.record ~loc:left.expression_loc attributes)
      | s ->
          Pinc_Diagnostics.error
            right.expression_loc
            ("Unknown property "
            ^ s
            ^ " on template node. Known properties are: `tag` and `attributes`."))
  | ComponentTemplateNode (tag, attributes, _), Ast.LowercaseIdentifierExpression b -> (
      match b with
      | "tag" ->
          state
          |> State.add_output ~output:(Helpers.Value.string ~loc:left.expression_loc tag)
      | "attributes" ->
          state
          |> State.add_output
               ~output:(Helpers.Value.record ~loc:left.expression_loc attributes)
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
  | DefinitionInfo (_, maybe_library, _), Ast.LowercaseIdentifierExpression b -> (
      match maybe_library with
      | Some (Definition_Library l) ->
          let output =
            l
            |> Type_Library.get_binding b
            |> Option.map (fun b -> b.value)
            |> Option.value
                 ~default:
                   (Helpers.Value.null
                      ~loc:
                        (Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                      ())
          in
          state |> State.add_output ~output
      | None ->
          state
          |> State.add_output
               ~output:
                 (Helpers.Value.null
                    ~loc:
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
          Helpers.Value.null
            ~loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
            ()
      in
      state |> State.add_output ~output
  | String a, Int b ->
      let output =
        try
          a
          |> CCUtf8_string.of_string_exn
          |> CCUtf8_string.to_list
          |> Fun.flip List.nth b
          |> Helpers.Value.char
               ~loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
        with Failure _ | Invalid_argument _ ->
          Helpers.Value.null
            ~loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
            ()
      in
      state |> State.add_output ~output
  | Record a, String b ->
      let output =
        a
        |> StringMap.find_opt b
        |> Option.value
             ~default:
               (Helpers.Value.null
                  ~loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
                  ())
      in
      state |> State.add_output ~output
  | Null, _ ->
      state
      |> State.add_output
           ~output:
             (Helpers.Value.null
                ~loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
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
        (Printf.sprintf
           "Trying to access a property on a non record or array value (%s)."
           (Value.to_string left_value))

and eval_binary_array_add ~state left right =
  let left_value = left |> eval_expression ~state |> State.get_output in
  let right_value = right |> eval_expression ~state |> State.get_output in
  match (left_value.value_desc, right_value) with
  | Array l, value ->
      let new_array = Array.append l [| value |] in
      state
      |> State.add_output
           ~output:
             (Helpers.Value.array
                ~loc:(Location.merge ~s:left.expression_loc ~e:right.expression_loc ())
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
        Helpers.Value.array
          ~loc:(Location.merge ~s:left.value_loc ~e:right.value_loc ())
          (Array.append l r)
    | Record l, Record r ->
        Helpers.Value.record
          ~loc:(Location.merge ~s:left.value_loc ~e:right.value_loc ())
          (StringMap.union (fun _key _x y -> Some y) l r)
    | HtmlTemplateNode (tag, attributes, children, self_closing), Record right ->
        let attributes = StringMap.union (fun _key _x y -> Some y) attributes right in
        {
          left with
          value_desc = HtmlTemplateNode (tag, attributes, children, self_closing);
        }
    | HtmlTemplateNode _, _ ->
        Pinc_Diagnostics.error
          right_expression.expression_loc
          "Trying to merge a non record value onto tag attributes."
    | ComponentTemplateNode _, _ ->
        Pinc_Diagnostics.error
          right_expression.expression_loc
          "Component attributes can't be modified."
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
          (Location.merge
             ~s:left_expression.expression_loc
             ~e:right_expression.expression_loc
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
             (Helpers.Value.bool
                ~loc:expression.expression_loc
                (not (Value.is_true expression_value)))

and eval_unary_minus ~state expression =
  let expression_value = eval_expression ~state expression |> State.get_output in
  match expression_value.value_desc with
  | Int i ->
      state
      |> State.add_output
           ~output:(Helpers.Value.int ~loc:expression.expression_loc (Int.neg i))
  | Float f ->
      state
      |> State.add_output
           ~output:(Helpers.Value.float ~loc:expression.expression_loc (Float.neg f))
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
  let requirement =
    if is_optional then
      `Optional
    else
      `Required
  in
  let state =
    expression
    |> eval_expression
         ~state:{ state with binding_identifier = Some (requirement, ident) }
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
      |> State.add_output ~output:(Helpers.Value.null ~loc:expression.expression_loc ())

and eval_use ~state ~ident expression =
  let value = expression |> eval_expression ~state |> State.get_output in
  match value with
  | { value_desc = DefinitionInfo (_, Some (Definition_Library library), _); _ } -> (
      match ident with
      | Some (Uppercase_Id ident) ->
          let ident, _ident_location = ident in
          state |> State.add_value_to_use_scope ~ident ~value:library
      | None ->
          let state =
            StringMap.fold
              (fun ident { is_optional; value; _ } ->
                State.add_value_to_scope ~ident ~is_mutable:false ~is_optional ~value)
              (Type_Library.get_bindings library)
              state
          in
          StringMap.fold
            (fun ident value -> State.add_value_to_use_scope ~ident ~value)
            (Type_Library.get_includes library)
            state)
  | _ ->
      Pinc_Diagnostics.error
        expression.expression_loc
        "Attempted to use a non library definition. \n\
         Expected to see a Library at the right hand side of the `use` statement."

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
      let requirement =
        if is_optional then
          `Optional
        else
          `Required
      in
      let output =
        expression
        |> eval_expression
             ~state:{ state with binding_identifier = Some (requirement, ident) }
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
      |> State.add_output ~output:(Helpers.Value.null ~loc:expression.expression_loc ())

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
             (Helpers.Value.null
                ~loc:
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
                   ~value:(Helpers.Value.int ~loc:ident_location !index)
                   ~is_mutable:false
                   ~is_optional:false
          | None -> state
        in
        match eval_expression ~state body with
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
          array |> Array.to_seq
      in
      let state, res = l |> to_seq |> loop ~state [] in
      state
      |> State.add_output ~output:(res |> Helpers.Value.list ~loc:body.expression_loc)
  | String s ->
      let map s =
        if reverse then
          s
          |> CCUtf8_string.to_seq
          |> CCSeq.to_rev_list
          |> List.to_seq
          |> Seq.map (fun c -> c |> Helpers.Value.char ~loc:iterable_value.value_loc)
        else
          s
          |> CCUtf8_string.to_seq
          |> Seq.map (fun c -> c |> Helpers.Value.char ~loc:iterable_value.value_loc)
      in
      let state, res = s |> CCUtf8_string.of_string_exn |> map |> loop ~state [] in
      state
      |> State.add_output ~output:(res |> Helpers.Value.list ~loc:body.expression_loc)
  | Portal l ->
      let to_seq list =
        if reverse then
          list |> List.to_seq
        else
          list |> List.rev |> List.to_seq
      in
      let state, res = l |> to_seq |> loop ~state [] in
      state
      |> State.add_output ~output:(res |> Helpers.Value.list ~loc:body.expression_loc)
  | Null -> state |> State.add_output ~output:iterable_value
  | HtmlTemplateNode _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over template node"
  | ComponentTemplateNode _ ->
      Pinc_Diagnostics.error iterable.expression_loc "Cannot iterate over template node"
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

and eval_range ~state ~inclusive from_expression upto_expression =
  let from = from_expression |> eval_expression ~state |> State.get_output in
  let upto = upto_expression |> eval_expression ~state |> State.get_output in
  let get_range from upto =
    match (from.value_desc, upto.value_desc) with
    | Int from, Int upto -> (from, upto)
    | Int from, Float upto when Float.is_integer upto -> (from, int_of_float upto)
    | Float from, Int upto when Float.is_integer from -> (int_of_float from, upto)
    | Float from, Float upto when Float.is_integer from && Float.is_integer upto ->
        (int_of_float from, int_of_float upto)
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
          Helpers.Value.int
            ~loc:(Location.merge ~s:from.value_loc ~e:upto.value_loc ())
            (i + start)))
  in
  state
  |> State.add_output
       ~output:
         (Helpers.Value.array
            ~loc:(Location.merge ~s:from.value_loc ~e:upto.value_loc ())
            iter)

and eval_block ~state statements =
  let state = state |> State.add_scope in
  let state = statements |> List.fold_left (fun state -> eval_statement ~state) state in
  state |> State.remove_scope

and eval_template ~state template =
  match template.template_node_desc with
  | Ast.TextTemplateNode text ->
      state
      |> State.add_output
           ~output:(Helpers.Value.string ~loc:template.template_node_loc text)
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
        let tag_data_provider ~tag ~attributes ~required ~key =
          (*
             TODO: Should we check the type here?
              ... Probably not, because we will implement
              a type checker which will do this at compile time anyways :)
          *)
          match tag with
          | Type_Tag.Tag_Store _ -> (
              component_tag_attributes
              |> StringMap.find_opt (key |> List.hd)
              |> Fun.flip Option.bind (Tag.find_path (key |> List.tl))
              |> function
              | None -> state.root_tag_data_provider ~tag ~attributes ~required ~key
              | value -> value)
          | Type_Tag.Tag_Slot _ ->
              let key = key |> List.rev |> List.hd in
              component_tag_children
              |> List.fold_left (Tag.Tag_Slot.keep_slotted ~key) []
              |> List.rev
              |> Helpers.Value.list
              |> Option.some
          | Type_Tag.Tag_Array ->
              let key = key |> List.rev |> List.hd in
              component_tag_attributes
              |> StringMap.find_opt key
              |> Fun.flip Option.bind (function
                     | { value_desc = Array a; _ } ->
                         a |> Array.length |> Helpers.Value.int |> Option.some
                     | _ -> None)
          | _ ->
              component_tag_attributes
              |> StringMap.find_opt (key |> List.hd)
              |> Fun.flip Option.bind (Tag.find_path (key |> List.tl))
        in
        let tag_meta_provider ~tag ~attributes ~required ~key =
          match tag with
          | Type_Tag.Tag_Store _ -> (
              component_tag_attributes
              |> StringMap.find_opt (key |> List.hd)
              |> Fun.flip Option.bind (Tag.find_path (key |> List.tl))
              |> function
              | None -> state.root_tag_meta_provider ~tag ~attributes ~required ~key
              | _ -> None)
          | _ -> None
        in

        let state =
          State.make
            ~context:state.context
            ~mode:state.mode
            ~tag_meta:state.tag_meta
            ~tag_data_provider
            ~root_tag_data_provider:state.root_tag_data_provider
            ~tag_meta_provider
            ~root_tag_meta_provider:state.root_tag_meta_provider
            state.declarations
        in
        DeclarationEvaluator.eval ~eval_expression ~state component_tag_identifier
        |> State.get_output
      in
      let result = render_fn component_tag_attributes in
      state
      |> State.add_output
           ~output:
             {
               value_loc = template.template_node_loc;
               value_desc =
                 ComponentTemplateNode
                   (component_tag_identifier, component_tag_attributes, result);
             }
;;

let declarations_of_sources =
  ListLabels.fold_left ~init:[] ~f:(fun acc source ->
      let decls = Parser.parse source in
      decls
      |> List.iter (fun (key, _) ->
             if List.mem_assoc key acc then (
               let loc =
                 let s =
                   Pinc_Diagnostics.Location.Position.make ~source ~line:0 ~column:0
                 in
                 Pinc_Diagnostics.Location.make ~s ()
               in

               let message =
                 Printf.sprintf
                   "Found multiple declarations with identifier `%s`.\n\
                    Every declaration has to have a unique name in pinc."
                   key
               in

               Pinc_Diagnostics.error loc message));
      acc @ decls)
;;

let eval_meta sources =
  let declarations =
    sources |> declarations_of_sources |> List.to_seq |> StringMap.of_seq
  in
  let state =
    State.make
      ~mode:`Portal_Collection
      ~tag_meta:[]
      ~root_tag_data_provider:Helpers.noop_data_provider
      ~tag_data_provider:Helpers.noop_data_provider
      ~root_tag_meta_provider:Helpers.noop_meta_provider
      ~tag_meta_provider:Helpers.noop_data_provider
      declarations
  in
  let eval attrs =
    attrs |> StringMap.map (fun e -> eval_expression ~state e |> State.get_output)
  in
  let open Ast in
  declarations
  |> StringMap.map (function
         | { declaration_type = Declaration_Component { declaration_attributes; _ }; _ }
           -> `Component (eval declaration_attributes)
         | { declaration_type = Declaration_Library { declaration_attributes; _ }; _ } ->
             `Library (eval declaration_attributes)
         | { declaration_type = Declaration_Page { declaration_attributes; _ }; _ } ->
             `Page (eval declaration_attributes)
         | { declaration_type = Declaration_Store { declaration_attributes; _ }; _ } ->
             `Store (eval declaration_attributes))
;;

let get_stdlib () =
  Pinc_stdlib.file_list
  |> List.map (fun filename ->
         filename |> Pinc_stdlib.read |> Option.get |> Source.of_string ~filename)
  |> List.fold_left
       (fun acc source ->
         let decls = source |> Parser.parse |> List.to_seq |> StringMap.of_seq in
         let f key _ _ =
           Pinc_Diagnostics.error
             (Pinc_Diagnostics.Location.make
                ~s:(Pinc_Diagnostics.Location.Position.make ~source ~line:0 ~column:0)
                ())
             ("Found multiple declarations with identifier " ^ key)
         in
         StringMap.union f acc decls)
       StringMap.empty
;;

let eval_declarations
    ?(tag_meta_provider = Helpers.noop_meta_provider)
    ~tag_data_provider
    ~root
    declarations =
  Hashtbl.reset Tag.Tag_Portal.portals;

  let declarations =
    declarations
    |> List.to_seq
    |> StringMap.of_seq
    |> StringMap.union (fun _key _x y -> Some y) (get_stdlib ())
  in

  (match declarations |> StringMap.find_opt root with
  | Some { Ast.declaration_type = Declaration_Library _ | Declaration_Store _; _ } ->
      raise_notrace (Invalid_argument (root ^ " can not be evaluated"))
  | _ -> ());

  let state =
    State.make
      ~root_tag_data_provider:tag_data_provider
      ~tag_data_provider
      ~root_tag_meta_provider:tag_meta_provider
      ~tag_meta_provider
      ~tag_meta:[]
      ~mode:`Portal_Collection
      declarations
  in
  let state = DeclarationEvaluator.eval ~eval_expression ~state root in

  let state =
    if Tag.Tag_Portal.portals |> Hashtbl.length > 0 then
      DeclarationEvaluator.eval
        ~eval_expression
        ~state:{ state with mode = `Portal_Render }
        root
    else
      state
  in

  let html = state |> State.get_output |> Value.to_string in
  let meta_tree = state.tag_meta in

  (html, meta_tree)
;;

let eval_sources ?tag_meta_provider ~tag_data_provider ~root sources =
  sources
  |> declarations_of_sources
  |> eval_declarations ?tag_meta_provider ~tag_data_provider ~root
;;
