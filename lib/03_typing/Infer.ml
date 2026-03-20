module Ast = Pinc_Parser.Ast
open Types
open Typed_tree

(* The state threaded through inference is a pair of the typing environment
   and the current accumulated substitution. The environment grows when new
   bindings are introduced (let, function parameters, for-in iterators).
   The substitution grows whenever unification produces a new binding. *)
type state = Environment.t * Substitution.t

let infer_type (_env, subst) ty =
  (* Apply the current substitution to a type to get its most resolved form *)
  Type.apply subst ty
;;

let unify (env, subst) loc t1 t2 =
  (* Unify two types and compose the result with the current substitution *)
  let t1' = Type.apply subst t1 in
  let t2' = Type.apply subst t2 in
  let new_subst =
    try Unify.most_general_unifier t1' t2'
    with Failure msg -> Pinc_Diagnostics.raise_type_error loc msg
  in
  (env, Substitution.compose new_subst subst)
;;

let bind_value (env, subst) name scheme =
  (* Extend the environment with a new value binding *)
  (StringMap.add name scheme env, subst)
;;

let rec infer_lowercase_id state = function
  | Ast.Lowercase_Id (name, loc) ->
      (state, T_Lowercase_Id (name, Type.fresh_variable (), loc))

and infer_uppercase_id state = function
  | Ast.Uppercase_Id (name, loc) ->
      (state, T_Uppercase_Id (name, Type.fresh_variable (), loc))

and infer_comment state comment = (state, Type.fresh_variable (), T_Comment comment)
and infer_char state c = (state, Type.TChar, T_Char c)
and infer_int state i = (state, Type.TInt, T_Int i)
and infer_float state f = (state, Type.TFloat, T_Float f)
and infer_bool state b = (state, Type.TBool, T_Bool b)

and infer_string state templates =
  let state, templates = List.fold_map ~init:state ~f:infer_string_template templates in
  (state, Type.TString, T_String templates)

and infer_string_template state (template : Ast.string_template) =
  let state, desc =
    match template.string_template_desc with
    | StringInterpolation id ->
        let state, id = infer_lowercase_id state id in
        (state, T_StringInterpolation id)
    | StringText s -> (state, T_StringText s)
  in
  ( state,
    {
      string_template_loc = template.string_template_loc;
      string_template_type = Type.fresh_variable ();
      string_template_desc = desc;
    } )

and infer_array state a =
  let state, array = Array.fold_map ~init:state ~f:infer_expression a in
  (state, Type.fresh_variable (), T_Array array)

and infer_record state r =
  let state, r =
    StringMap.fold_mapi r ~init:state ~f:(fun _key state (requirement, expr) ->
        let state, expr = infer_expression state expr in
        (state, (requirement, expr)))
  in
  (state, Type.fresh_variable (), T_Record r)

and infer_external_function state identifier parameters name =
  let state, identifier = infer_lowercase_id state identifier in
  let state, parameters = List.fold_map ~init:state ~f:infer_lowercase_id parameters in
  (state, Type.fresh_variable (), T_ExternalFunction { identifier; parameters; name })

and infer_function state identifier parameters body =
  (* 1. Create a fresh tvar for each parameter *)
  let param_tvars = List.map (fun _ -> Type.fresh_variable ()) parameters in

  (* 2. Add parameters to env as monomorphic schemes *)
  let state =
    List.fold_left2
      (fun state (Ast.Lowercase_Id (name, _)) tv ->
        bind_value state name (Scheme.Scheme ([], tv)))
      state
      parameters
      param_tvars
  in

  (* 3. If named, add the function itself to env as monomorphic for recursion.
        Use a fresh tvar, because its type isn't known yet *)
  let state, identifier =
    match identifier with
    | None -> (state, None)
    | Some (Ast.Lowercase_Id (name, _) as id) ->
        let self_tv = Type.fresh_variable () in
        let state = bind_value state name (Scheme.Scheme ([], self_tv)) in
        let state, typed_id = infer_lowercase_id state id in
        (state, Some typed_id)
  in

  (* 4. Infer the body *)
  let state, typed_body = infer_expression state body in
  let body_type = typed_body.expression_type in
  let _, subst = state in

  (* 5. Build the function type: resolve each param tvar through substitution *)
  let param_types = List.map (Type.apply subst) param_tvars in
  let typed_params =
    List.map2
      (fun (Ast.Lowercase_Id (name, loc)) ty -> T_Lowercase_Id (name, ty, loc))
      parameters
      param_types
  in

  ( state,
    Type.TFunction (param_types, body_type),
    T_Function { identifier; parameters = typed_params; body = typed_body } )

and infer_function_call state function_definition arguments =
  let state, function_definition = infer_expression state function_definition in
  let state, arguments = List.fold_map ~init:state ~f:infer_expression arguments in
  (state, Type.fresh_variable (), T_FunctionCall { function_definition; arguments })

and infer_uppercase_id_path_expression state path =
  let env, subst = state in
  match path with
  | [] -> assert false
  | hd :: tl ->
      let head_ty =
        match StringMap.find_opt hd env with
        | None ->
            Pinc_Diagnostics.raise_type_error
              Pinc_Diagnostics.Location.none
              (Printf.sprintf "Unbound declaration `%s`" hd)
        | Some scheme -> Generalize.instantiate scheme |> Type.apply subst
      in
      let ty =
        List.fold_left
          (fun ty segment ->
            match ty with
            | Type.TLibrary fields -> (
                match StringMap.find_opt segment fields with
                | Some t -> Type.apply subst t
                | None ->
                    Pinc_Diagnostics.raise_type_error
                      Pinc_Diagnostics.Location.none
                      (Printf.sprintf "Library has no member `%s`" segment))
            | Type.TVar _ -> Type.fresh_variable ()
            | t ->
                Pinc_Diagnostics.raise_type_error
                  Pinc_Diagnostics.Location.none
                  (Printf.sprintf
                     "Expected a library when navigating path, got %s"
                     (Type.show t)))
          head_ty
          tl
      in
      (state, ty, T_UppercaseIdentifierPathExpression path)

and infer_uppercase_id_expression state name =
  let state, id =
    infer_uppercase_id state (Ast.Uppercase_Id (name, Pinc_Diagnostics.Location.none))
  in
  (state, Type.fresh_variable (), T_UppercaseIdentifierExpression id)

and infer_lowercase_id_expression state loc name =
  let env, subst = state in
  match StringMap.find_opt name env with
  | None ->
      let message = Printf.sprintf "Unbound identifier `%s`" name in
      Pinc_Diagnostics.raise_type_error loc message
  | Some scheme ->
      let ty = Generalize.instantiate scheme |> Type.apply subst in
      let id = T_Lowercase_Id (name, ty, loc) in
      (state, ty, T_LowercaseIdentifierExpression id)

and infer_tag state (tag : Ast.tag) =
  let state, tag_desc = infer_tag_desc state tag.tag_desc in
  ( state,
    Type.fresh_variable (),
    T_TagExpression { tag_loc = tag.tag_loc; tag_type = Type.fresh_variable (); tag_desc }
  )

and infer_tag_desc state (desc : Ast.tag_desc) =
  let tag_kind =
    match desc.tag with
    | Tag_String -> T_Tag_String
    | Tag_Int -> T_Tag_Int
    | Tag_Float -> T_Tag_Float
    | Tag_Boolean -> T_Tag_Boolean
    | Tag_Array -> T_Tag_Array
    | Tag_Record -> T_Tag_Record
    | Tag_Slot -> T_Tag_Slot
    | Tag_Store -> T_Tag_Store
    | Tag_SetContext -> T_Tag_SetContext
    | Tag_GetContext -> T_Tag_GetContext
    | Tag_CreatePortal -> T_Tag_CreatePortal
    | Tag_Portal -> T_Tag_Portal
    | Tag_Custom name -> T_Tag_Custom name
  in
  let state, attributes =
    StringMap.fold_map ~init:state ~f:infer_expression desc.attributes
  in
  let state, transformer =
    Option.fold_map ~init:state ~f:infer_expression desc.transformer
  in
  let state, children = Option.fold_map ~init:state ~f:infer_expression desc.children in
  ( state,
    {
      tag = tag_kind;
      key = desc.key;
      required = desc.required;
      attributes;
      transformer;
      children;
    } )

and infer_for_in state ~index ~iterator ~reverse ~iterable ~body =
  let state, iterable = infer_expression state iterable in

  let state, index =
    Option.fold_map index ~init:state ~f:(fun state (Ast.Lowercase_Id (name, loc)) ->
        let state = bind_value state name (Scheme.Scheme ([], Type.TInt)) in
        (state, T_Lowercase_Id (name, Type.TInt, loc)))
  in

  let state, iterator =
    let (Ast.Lowercase_Id (name, loc)) = iterator in
    let tv = Type.fresh_variable () in
    let state = bind_value state name (Scheme.Scheme ([], tv)) in
    (state, T_Lowercase_Id (name, tv, loc))
  in

  let state, body = infer_expression state body in
  ( state,
    (* TODO: The type should be an array of whatever the body returns *)
    Type.fresh_variable (),
    T_ForInExpression { index; iterator; reverse; iterable; body } )

and infer_conditional state ~condition ~consequent ~alternate =
  let state, condition = infer_expression state condition in
  let state, consequent = infer_statement state consequent in
  let state, alternate = Option.fold_map ~init:state ~f:infer_statement alternate in
  ( state,
    Type.fresh_variable (),
    T_ConditionalExpression { condition; consequent; alternate } )

and infer_block state statements =
  let state =
    List.fold_left
      (fun state stmt ->
        match stmt.Ast.statement_desc with
        | LetStatement (Ast.Lowercase_Id (name, _), _)
        | MutableLetStatement (Ast.Lowercase_Id (name, _), _)
        | OptionalLetStatement (Ast.Lowercase_Id (name, _), _)
        | OptionalMutableLetStatement (Ast.Lowercase_Id (name, _), _) ->
            let tv = Type.fresh_variable () in
            bind_value state name (Scheme.Scheme ([], tv))
        | _ -> state)
      state
      statements
  in
  let state, statements = List.fold_map ~init:state ~f:infer_statement statements in
  (state, Type.fresh_variable (), T_BlockExpression statements)

and infer_template state nodes =
  let state, nodes = List.fold_map ~init:state ~f:infer_template_node nodes in
  (state, Type.fresh_variable (), T_TemplateExpression nodes)

and infer_unary_expression state op expr =
  let state, expr = infer_expression state expr in
  (state, Type.fresh_variable (), T_UnaryExpression (op, expr))

and assert_numeric state loc ty =
  let _, subst = state in
  match Type.apply subst ty with
  | Type.TInt | Type.TFloat | Type.TChar -> state
  | Type.TVar _ ->
      (* Not yet resolved — cannot check yet, will be caught later if wrong *)
      state
  | t ->
      Pinc_Diagnostics.raise_type_error
        loc
        (Printf.sprintf
           "Expected a numeric type (int, float or char), got %s"
           (Type.show t))

and numeric_result_type state left_ty right_ty =
  let _, subst = state in
  match (Type.apply subst left_ty, Type.apply subst right_ty) with
  | Type.TInt, Type.TInt -> Type.TInt
  | Type.TFloat, _ | _, Type.TFloat -> Type.TFloat
  | _ -> Type.TInt (* both still TVar — default to int, will resolve later *)

and infer_binary_expression state left op right =
  let open Pinc_Parser.Ast.Operators.Binary in
  match op with
  (* Numeric arithmetic: both sides int or float, result follows widening rules *)
  | PLUS | MINUS | TIMES ->
      let state, left = infer_expression state left in
      let state = assert_numeric state left.expression_loc left.expression_type in
      let state, right = infer_expression state right in
      let state = assert_numeric state right.expression_loc right.expression_type in
      let result_ty =
        numeric_result_type state left.expression_type right.expression_type
      in
      (state, result_ty, T_BinaryExpression (left, op, right))
  (* Power: numeric in, float out — matches interpreter behaviour *)
  | POW ->
      let state, left = infer_expression state left in
      let state = assert_numeric state left.expression_loc left.expression_type in
      let state, right = infer_expression state right in
      let state = assert_numeric state right.expression_loc right.expression_type in
      (state, Type.TFloat, T_BinaryExpression (left, op, right))
  (* Modulo: integers only *)
  | MODULO ->
      let state, left = infer_expression state left in
      let state = unify state left.expression_loc left.expression_type Type.TInt in
      let state, right = infer_expression state right in
      let state = unify state right.expression_loc right.expression_type Type.TInt in
      (state, Type.TInt, T_BinaryExpression (left, op, right))
  (* Division: numeric in, always float out — matches interpreter behaviour *)
  | DIV ->
      let state, left = infer_expression state left in
      let state = assert_numeric state left.expression_loc left.expression_type in
      let state, right = infer_expression state right in
      let state = assert_numeric state right.expression_loc right.expression_type in
      (state, Type.TFloat, T_BinaryExpression (left, op, right))
  (* Concatenation: both sides string, returns string *)
  | CONCAT ->
      let state, left = infer_expression state left in
      let state = unify state left.expression_loc left.expression_type Type.TString in
      let state, right = infer_expression state right in
      let state = unify state right.expression_loc right.expression_type Type.TString in
      (state, Type.TString, T_BinaryExpression (left, op, right))
  (* Comparison: both sides must unify, returns bool *)
  | EQUAL | NOT_EQUAL | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ->
      let state, left = infer_expression state left in
      let state, right = infer_expression state right in
      let state =
        unify state right.expression_loc left.expression_type right.expression_type
      in
      (state, Type.TBool, T_BinaryExpression (left, op, right))
  (* Logical: both sides bool, returns bool *)
  | AND | OR ->
      let state, left = infer_expression state left in
      let state = unify state left.expression_loc left.expression_type Type.TBool in
      let state, right = infer_expression state right in
      let state = unify state right.expression_loc right.expression_type Type.TBool in
      (state, Type.TBool, T_BinaryExpression (left, op, right))
  (* Dot access: left must be TRecord or TLibrary, right is a field name label *)
  | DOT_ACCESS ->
      let state, left = infer_expression state left in
      let _, subst = state in
      let left_ty = Type.apply subst left.expression_type in
      (* The right side is always a label — extract the name without looking it up in env *)
      let field_name =
        match right.expression_desc with
        | LowercaseIdentifierExpression name -> name
        | _ ->
            Pinc_Diagnostics.raise_type_error
              right.expression_loc
              "Expected a field name on the right side of dot access"
      in
      let field_ty =
        let lookup_field fields loc =
          match StringMap.find_opt field_name fields with
          | Some t -> Type.apply subst t
          | None ->
              Pinc_Diagnostics.raise_type_error
                loc
                (Printf.sprintf "No field `%s` found" field_name)
        in
        match left_ty with
        | Type.TRecord fields -> lookup_field fields right.expression_loc
        | Type.TLibrary fields -> lookup_field fields right.expression_loc
        | Type.TComponent fields -> lookup_field fields right.expression_loc
        | Type.TStore fields -> lookup_field fields right.expression_loc
        | Type.TVar _ ->
            (* Left side not yet resolved — return fresh and let unification handle it *)
            Type.fresh_variable ()
        | t ->
            Pinc_Diagnostics.raise_type_error
              left.expression_loc
              (Printf.sprintf
                 "Expected a record or library on the left side of dot access, got %s"
                 (Type.show t))
      in
      (* Build a typed node for the right side using the looked-up type,
         without going through infer_lowercase_id_expression *)
      let typed_right =
        {
          expression_loc = right.expression_loc;
          expression_type = field_ty;
          expression_desc =
            T_LowercaseIdentifierExpression
              (T_Lowercase_Id (field_name, field_ty, right.expression_loc));
        }
      in
      (state, field_ty, T_BinaryExpression (left, op, typed_right))
  (* Bracket access: array[int] -> element, string[int] -> char *)
  | BRACKET_ACCESS ->
      let state, left = infer_expression state left in
      let state, right = infer_expression state right in
      let _, subst = state in
      let left_ty = Type.apply subst left.expression_type in
      let result_ty =
        match left_ty with
        | Type.TString -> Type.TChar
        | _ -> Type.fresh_variable ()
      in
      (state, result_ty, T_BinaryExpression (left, op, right))
  (* Array add: array <- element, returns same array type *)
  | ARRAY_ADD ->
      let state, left = infer_expression state left in
      let state, right = infer_expression state right in
      (state, left.expression_type, T_BinaryExpression (left, op, right))
  (* Merge: two records merged *)
  | MERGE ->
      let state, left = infer_expression state left in
      let state, right = infer_expression state right in
      (state, Type.fresh_variable (), T_BinaryExpression (left, op, right))
  (* Range: int..int *)
  | RANGE | INCLUSIVE_RANGE ->
      let state, left = infer_expression state left in
      let state = unify state left.expression_loc left.expression_type Type.TInt in
      let state, right = infer_expression state right in
      let state = unify state right.expression_loc right.expression_type Type.TInt in
      (state, Type.fresh_variable (), T_BinaryExpression (left, op, right))
  (* Pipe: e1 |> f *)
  | PIPE ->
      let state, left = infer_expression state left in
      let state, right = infer_expression state right in
      (state, Type.fresh_variable (), T_BinaryExpression (left, op, right))
  (* Function call via operator *)
  | FUNCTION_CALL ->
      let state, left = infer_expression state left in
      let state, right = infer_expression state right in
      (state, Type.fresh_variable (), T_BinaryExpression (left, op, right))

and infer_html_template_node
    state
    ~html_tag_identifier
    ~html_tag_attributes
    ~html_tag_children
    ~html_tag_self_closing =
  let state, html_tag_attributes =
    StringMap.fold_map ~init:state ~f:infer_expression html_tag_attributes
  in
  let state, html_tag_children =
    List.fold_map ~init:state ~f:infer_template_node html_tag_children
  in
  ( state,
    T_HtmlTemplateNode
      {
        html_tag_identifier;
        html_tag_attributes;
        html_tag_children;
        html_tag_self_closing;
      } )

and infer_component_template_node
    state
    ~component_tag_identifier
    ~component_tag_attributes
    ~component_tag_children =
  let state, component_tag_identifier =
    infer_uppercase_id state component_tag_identifier
  in
  let state, component_tag_attributes =
    StringMap.fold_map ~init:state ~f:infer_expression component_tag_attributes
  in
  let state, component_tag_children =
    List.fold_map ~init:state ~f:infer_template_node component_tag_children
  in
  ( state,
    T_ComponentTemplateNode
      { component_tag_identifier; component_tag_attributes; component_tag_children } )

and infer_template_node state (node : Ast.template_node) =
  let state, desc =
    match node.template_node_desc with
    | HtmlTemplateNode
        {
          html_tag_identifier;
          html_tag_attributes;
          html_tag_children;
          html_tag_self_closing;
        } ->
        infer_html_template_node
          state
          ~html_tag_identifier
          ~html_tag_attributes
          ~html_tag_children
          ~html_tag_self_closing
    | ComponentTemplateNode
        { component_tag_identifier; component_tag_attributes; component_tag_children } ->
        infer_component_template_node
          state
          ~component_tag_identifier
          ~component_tag_attributes
          ~component_tag_children
    | ExpressionTemplateNode expr ->
        let state, expr = infer_expression state expr in
        (state, T_ExpressionTemplateNode expr)
    | TextTemplateNode s -> (state, T_TextTemplateNode s)
  in
  ( state,
    {
      template_node_loc = node.template_node_loc;
      template_node_type = Type.fresh_variable ();
      template_node_desc = desc;
    } )

and infer_expression state (expr : Ast.expression) =
  let state, ty, desc =
    match expr.expression_desc with
    | Comment comment -> infer_comment state comment
    | String templates -> infer_string state templates
    | Char c -> infer_char state c
    | Int i -> infer_int state i
    | Float f -> infer_float state f
    | Bool b -> infer_bool state b
    | Array a -> infer_array state a
    | Record r -> infer_record state r
    | ExternalFunction { identifier; parameters; name } ->
        infer_external_function state identifier parameters name
    | Function { identifier; parameters; body } ->
        infer_function state identifier parameters body
    | FunctionCall { function_definition; arguments } ->
        infer_function_call state function_definition arguments
    | UppercaseIdentifierPathExpression path ->
        infer_uppercase_id_path_expression state path
    | UppercaseIdentifierExpression name -> infer_uppercase_id_expression state name
    | LowercaseIdentifierExpression name ->
        infer_lowercase_id_expression state expr.expression_loc name
    | TagExpression tag -> infer_tag state tag
    | ForInExpression { index; iterator; reverse; iterable; body } ->
        infer_for_in state ~index ~iterator ~reverse ~iterable ~body
    | ConditionalExpression { condition; consequent; alternate } ->
        infer_conditional state ~condition ~consequent ~alternate
    | BlockExpression statements -> infer_block state statements
    | TemplateExpression nodes -> infer_template state nodes
    | UnaryExpression (op, expr) -> infer_unary_expression state op expr
    | BinaryExpression (left, op, right) -> infer_binary_expression state left op right
  in
  let _, subst = state in
  ( state,
    {
      expression_loc = expr.expression_loc;
      expression_type = Type.apply subst ty;
      expression_desc = desc;
    } )

and infer_comment_stmt state s = (state, T_CommentStatement s)
and infer_break_stmt state s = (state, T_BreakStatement s)
and infer_continue_stmt state s = (state, T_ContinueStatement s)

and infer_use_stmt state id expr =
  let env, subst = state in
  (* Resolve any uppercase expression (plain name or path) to a type *)
  let resolve_decl_expr loc =
    match expr.Ast.expression_desc with
    | UppercaseIdentifierExpression name -> (
        match StringMap.find_opt name env with
        | None ->
            Pinc_Diagnostics.raise_type_error
              loc
              (Printf.sprintf "Unbound declaration `%s`" name)
        | Some scheme -> Generalize.instantiate scheme |> Type.apply subst)
    | UppercaseIdentifierPathExpression path -> (
        match path with
        | [] -> assert false
        | hd :: tl ->
            let head_ty =
              match StringMap.find_opt hd env with
              | None ->
                  Pinc_Diagnostics.raise_type_error
                    loc
                    (Printf.sprintf "Unbound declaration `%s`" hd)
              | Some scheme -> Generalize.instantiate scheme |> Type.apply subst
            in
            List.fold_left
              (fun ty segment ->
                match ty with
                | Type.TLibrary fields -> (
                    match StringMap.find_opt segment fields with
                    | Some t -> Type.apply subst t
                    | None ->
                        Pinc_Diagnostics.raise_type_error
                          loc
                          (Printf.sprintf "Library has no member `%s`" segment))
                | Type.TVar _ -> Type.fresh_variable ()
                | t ->
                    Pinc_Diagnostics.raise_type_error
                      loc
                      (Printf.sprintf
                         "Expected a library when navigating path, got %s"
                         (Type.show t)))
              head_ty
              tl)
    | _ ->
        Pinc_Diagnostics.raise_type_error
          loc
          "Expected a declaration name or path in use statement"
  in
  let state =
    match id with
    | Some (Ast.Uppercase_Id (alias, loc)) ->
        let ty = resolve_decl_expr loc in
        bind_value state alias (Scheme.Scheme ([], ty))
    | None -> (
        let ty = resolve_decl_expr expr.Ast.expression_loc in
        match ty with
        | Type.TLibrary fields | Type.TComponent fields | Type.TStore fields ->
            StringMap.fold
              (fun name field_ty s -> bind_value s name (Scheme.Scheme ([], field_ty)))
              fields
              state
        | _ -> state)
  in
  let state, id = Option.fold_map ~init:state ~f:infer_uppercase_id id in
  let state, expr = infer_expression state expr in
  (state, T_UseStatement (id, expr))

and infer_optional_mutable_let state id expr =
  let env, _substitution = state in
  let (Ast.Lowercase_Id (identifier_name, identifier_loc)) = id in
  let state, typed_expr = infer_expression state expr in
  let _, substitution = state in
  let expr_type = Type.apply substitution typed_expr.expression_type in
  let scheme = Generalize.generalize env expr_type in
  let state = bind_value state identifier_name scheme in
  let typed_id = T_Lowercase_Id (identifier_name, expr_type, identifier_loc) in
  (state, T_OptionalMutableLetStatement (typed_id, typed_expr))

and infer_optional_let state id expr =
  let env, _substitution = state in
  let (Ast.Lowercase_Id (identifier_name, identifier_loc)) = id in
  let state, typed_expr = infer_expression state expr in
  let _, substitution = state in
  let expr_type = Type.apply substitution typed_expr.expression_type in
  let scheme = Generalize.generalize env expr_type in
  let state = bind_value state identifier_name scheme in
  let typed_id = T_Lowercase_Id (identifier_name, expr_type, identifier_loc) in
  (state, T_OptionalLetStatement (typed_id, typed_expr))

and infer_mutable_let state id expr =
  let env, _substitution = state in
  let (Ast.Lowercase_Id (identifier_name, identifier_loc)) = id in
  let state, typed_expr = infer_expression state expr in
  let _, substitution = state in
  let expr_type = Type.apply substitution typed_expr.expression_type in
  let scheme = Generalize.generalize env expr_type in
  let state = bind_value state identifier_name scheme in
  let typed_id = T_Lowercase_Id (identifier_name, expr_type, identifier_loc) in
  (state, T_MutableLetStatement (typed_id, typed_expr))

and infer_let state id expr =
  let env, _substitution = state in
  let (Ast.Lowercase_Id (identifier_name, identifier_loc)) = id in
  let state, typed_expr = infer_expression state expr in
  let _, substitution = state in
  let expr_type = Type.apply substitution typed_expr.expression_type in
  let scheme = Generalize.generalize env expr_type in
  let state = bind_value state identifier_name scheme in
  let typed_id = T_Lowercase_Id (identifier_name, expr_type, identifier_loc) in
  (state, T_LetStatement (typed_id, typed_expr))

and infer_mutation state id expr =
  let state, id = infer_lowercase_id state id in
  let state, expr = infer_expression state expr in
  (state, T_MutationStatement (id, expr))

and infer_expression_stmt state expr =
  let state, expr = infer_expression state expr in
  (state, T_ExpressionStatement expr)

and infer_statement state (statement : Ast.statement) =
  let state, desc =
    match statement.statement_desc with
    | CommentStatement s -> infer_comment_stmt state s
    | BreakStatement s -> infer_break_stmt state s
    | ContinueStatement s -> infer_continue_stmt state s
    | UseStatement (id, expr) -> infer_use_stmt state id expr
    | OptionalMutableLetStatement (id, expr) -> infer_optional_mutable_let state id expr
    | OptionalLetStatement (id, expr) -> infer_optional_let state id expr
    | MutableLetStatement (id, expr) -> infer_mutable_let state id expr
    | LetStatement (id, expr) -> infer_let state id expr
    | MutationStatement (id, expr) -> infer_mutation state id expr
    | ExpressionStatement expr -> infer_expression_stmt state expr
  in
  let _, subst = state in
  ( state,
    {
      statement_loc = statement.statement_loc;
      statement_type = Type.apply subst (Type.fresh_variable ());
      statement_desc = desc;
    } )

and infer_declaration_desc state (desc : Ast.declaration_desc) =
  let state, declaration_attributes =
    StringMap.fold_map ~init:state ~f:infer_expression desc.declaration_attributes
  in
  let state, declaration_body = infer_expression state desc.declaration_body in
  (state, { declaration_attributes; declaration_body })

(* After inferring a declaration body, collect the bindings that were added
   to the env during that inference and build a fields map for the T* type.
   We compute the diff between the env before and after inference. *)
and collect_new_bindings ~env_before state =
  let env_after, subst = state in
  StringMap.fold
    (fun name _scheme acc ->
      if StringMap.mem name env_before then
        acc
        (* was already in scope before — not a new export *)
      else (
        let ty =
          match StringMap.find_opt name env_after with
          | Some scheme -> Generalize.instantiate scheme |> Type.apply subst
          | None -> Type.fresh_variable ()
        in
        StringMap.add name ty acc))
    env_after
    StringMap.empty

and infer_library_declaration outer_state name desc =
  (* Infer the body in a fresh child env so its internal bindings don't
     leak into the outer scope *)
  let env_before, subst = outer_state in
  let child_state = (env_before, subst) in
  let child_state, typed_desc = infer_declaration_desc child_state desc in
  let fields = collect_new_bindings ~env_before child_state in
  let lib_ty = Type.TLibrary fields in
  (* Bind the library name in the outer env *)
  let outer_state = bind_value outer_state name (Scheme.Scheme ([], lib_ty)) in
  (outer_state, lib_ty, T_Declaration_Library typed_desc)

and infer_component_declaration outer_state name desc =
  let env_before, subst = outer_state in
  let child_state = (env_before, subst) in
  let child_state, typed_desc = infer_declaration_desc child_state desc in
  let fields = collect_new_bindings ~env_before child_state in
  let comp_ty = Type.TComponent fields in
  let outer_state = bind_value outer_state name (Scheme.Scheme ([], comp_ty)) in
  (outer_state, comp_ty, T_Declaration_Component typed_desc)

and infer_page_declaration outer_state name desc =
  let env_before, subst = outer_state in
  let child_state = (env_before, subst) in
  let child_state, typed_desc = infer_declaration_desc child_state desc in
  let fields = collect_new_bindings ~env_before child_state in
  let page_ty = Type.TPage fields in
  let outer_state = bind_value outer_state name (Scheme.Scheme ([], page_ty)) in
  (outer_state, page_ty, T_Declaration_Page typed_desc)

and infer_store_declaration outer_state name desc =
  let env_before, subst = outer_state in
  let child_state = (env_before, subst) in
  let child_state, typed_desc = infer_declaration_desc child_state desc in
  let fields = collect_new_bindings ~env_before child_state in
  let store_ty = Type.TStore fields in
  let outer_state = bind_value outer_state name (Scheme.Scheme ([], store_ty)) in
  (outer_state, store_ty, T_Declaration_Store typed_desc)

and infer_declaration state ((name, declaration) : string * Ast.declaration) =
  let state, decl_ty, declaration_kind =
    match declaration.declaration_kind with
    | Declaration_Component desc -> infer_component_declaration state name desc
    | Declaration_Library desc -> infer_library_declaration state name desc
    | Declaration_Page desc -> infer_page_declaration state name desc
    | Declaration_Store desc -> infer_store_declaration state name desc
  in
  let _, subst = state in
  ( state,
    {
      declaration_loc = declaration.declaration_loc;
      declaration_type = Type.apply subst decl_ty;
      declaration_kind;
    } )

and infer_group ast state group =
  match group with
  | Pinc_Parser.DependencyGraph.Single name -> (
      match StringMap.find_opt name ast with
      | None -> (state, StringMap.empty)
      | Some decl ->
          let state, typed_decl = infer_declaration state (name, decl) in
          (state, StringMap.singleton name typed_decl))
  | Pinc_Parser.DependencyGraph.Recursive names ->
      (* Seed all names in the group with fresh tvars before inferring any —
         handles mutual recursion at the declaration level *)
      let state =
        List.fold_left
          (fun state name ->
            let tv = Type.fresh_variable () in
            bind_value state name (Scheme.Scheme ([], tv)))
          state
          names
      in
      (* Infer all declarations in the group *)
      let state, typed_map =
        List.fold_left
          (fun (state, acc) name ->
            match StringMap.find_opt name ast with
            | None -> (state, acc)
            | Some decl ->
                let state, typed_decl = infer_declaration state (name, decl) in
                (state, StringMap.add name typed_decl acc))
          (state, StringMap.empty)
          names
      in
      (state, typed_map)

and infer_declarations ast graph state =
  let groups = Pinc_Parser.DependencyGraph.topo_sorted_groups graph in
  List.fold_left
    (fun (state, acc) group ->
      let state, typed_map = infer_group ast state group in
      (state, StringMap.union (fun _ a _ -> Some a) acc typed_map))
    (state, StringMap.empty)
    groups
;;

let infer (ast : Ast.t) : Typed_tree.t =
  let graph = Pinc_Parser.DependencyGraph.build ast in
  let state = (Environment.empty, Substitution.empty) in
  let _state, typed_tree = infer_declarations ast graph state in
  typed_tree
;;
