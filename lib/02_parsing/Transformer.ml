open Utils
open Ast

type public_tag = {
  public_tag_key : string;
  public_tag_typ : tag_typ;
  public_tag_children : public_tag list option;
}

module Env = struct
  type t = {
    tags : public_tag list;
    current_identifier :
      ([ `Required | `Optional ] * Parsetree.lowercase_identifier) option;
  }

  let empty = { tags = []; current_identifier = None }
end

let rec transform_lowercase_id env = function
  | Parsetree.P_Lowercase_Id (id, loc) -> (env, Lowercase_Id (id, loc))

and transform_uppercase_id env = function
  | Parsetree.P_Uppercase_Id (id, loc) -> (env, Uppercase_Id (id, loc))

and transform_comment env comment = (env, Comment comment)

and transform_string env templates =
  let transform_string_template env (template : Parsetree.string_template) =
    let env, desc =
      match template.string_template_desc with
      | P_StringInterpolation identifier ->
          let env, id = transform_lowercase_id env identifier in
          (env, StringInterpolation id)
      | P_StringText s -> (env, StringText s)
    in
    ( env,
      { string_template_loc = template.string_template_loc; string_template_desc = desc }
    )
  in
  let env, s = List.fold_map ~init:env ~f:transform_string_template templates in
  (env, String s)

and transform_char env c = (env, Char c)
and transform_int env i = (env, Int i)
and transform_float env f = (env, Float f)
and transform_bool env b = (env, Bool b)

and transform_array env a =
  let env, array = Array.fold_map ~init:env ~f:transform_expression a in
  (env, Array array)

and transform_record env r =
  let env, r =
    StringMap.fold_mapi r ~init:env ~f:(fun key env (requirement, expr) ->
        let env =
          {
            env with
            Env.current_identifier =
              Some
                ( requirement,
                  Parsetree.P_Lowercase_Id (key, Pinc_Diagnostics.Location.none) );
          }
        in
        let env, expr = transform_expression env expr in
        let result = (requirement, expr) in
        let env = { env with Env.current_identifier = None } in
        (env, result))
  in
  (env, Record r)

and transform_external_function env ~loc parameters name =
  let identifier = env.Env.current_identifier in
  match identifier with
  | None -> Pinc_Diagnostics.error loc "This function is missing an identifier."
  | Some (_, identifier) ->
      let env, identifier = transform_lowercase_id env identifier in
      (env, ExternalFunction { identifier; parameters; name })

and transform_function env parameters body =
  let env, identifier =
    match env.Env.current_identifier with
    | None -> (env, None)
    | Some (_, identifier) ->
        let env, id = transform_lowercase_id env identifier in
        (env, Some id)
  in
  let env, body = transform_expression env body in
  (env, Function { identifier; parameters; body })

and transform_function_call env function_definition arguments =
  let env, function_definition = transform_expression env function_definition in
  let env, arguments = List.fold_map ~init:env ~f:transform_expression arguments in
  (env, FunctionCall { function_definition; arguments })

and transform_uppercase_id_path_expression env path =
  (env, UppercaseIdentifierPathExpression path)

and transform_uppercase_id_expression env id = (env, UppercaseIdentifierExpression id)
and transform_lowercase_id_expression env id = (env, LowercaseIdentifierExpression id)

and transform_tag env (tag : Parsetree.tag) =
  let transform_tag_desc env (desc : Parsetree.tag_desc) =
    let current_identifier = env.Env.current_identifier in
    let public, tag =
      match desc.tag with
      | P_Tag_String -> (true, Tag_String)
      | P_Tag_Int -> (true, Tag_Int)
      | P_Tag_Float -> (true, Tag_Float)
      | P_Tag_Boolean -> (true, Tag_Boolean)
      | P_Tag_Array -> (true, Tag_Array)
      | P_Tag_Record -> (true, Tag_Record)
      | P_Tag_Slot -> (true, Tag_Slot)
      | P_Tag_Store -> (true, Tag_Store)
      | P_Tag_SetContext -> (false, Tag_SetContext)
      | P_Tag_GetContext -> (false, Tag_GetContext)
      | P_Tag_CreatePortal -> (false, Tag_CreatePortal)
      | P_Tag_Portal -> (false, Tag_Portal)
      | P_Tag_Custom s -> (true, Tag_Custom s)
    in
    let child_env, children =
      desc.attributes
      |> StringMap.find_opt "of"
      |> Option.fold_map
           ~init:
             {
               Env.tags = [];
               Env.current_identifier =
                 (match tag with
                 | Tag_Array ->
                     Some
                       ( `Optional,
                         Parsetree.P_Lowercase_Id ("#", Pinc_Diagnostics.Location.none) )
                 | _ -> None);
             }
           ~f:transform_expression
    in
    let env, attributes =
      desc.attributes
      |> StringMap.remove "of"
      |> StringMap.fold_map ~init:env ~f:transform_expression
    in
    let env, transformer =
      Option.fold_map ~init:env ~f:transform_expression desc.transformer
    in

    let key =
      attributes
      |> StringMap.find_opt "key"
      |> Option.map (function
           | { expression_desc = String []; expression_loc = _ } -> ""
           | {
               expression_desc = String [ { string_template_desc = StringText s; _ } ];
               expression_loc = loc;
             } ->
               if Helpers.is_valid_lowercase_ident s then
                 s
               else
                 Pinc_Diagnostics.error
                   loc
                   "Tag keys may only contain ASCII characters (a-Z), numbers (0-9) and \
                    underscores (_)."
           | { expression_desc = String _; expression_loc = loc } ->
               Pinc_Diagnostics.error
                 loc
                 "Expected attribute `key` on tag to be a plain string without \
                  interpolations."
           | { expression_desc = _; expression_loc = loc } ->
               Pinc_Diagnostics.error
                 loc
                 "Expected attribute `key` on tag to be of type string")
    in
    let key =
      match (key, current_identifier) with
      | None, Some (_, Parsetree.P_Lowercase_Id (ident, _)) -> ident
      | Some s, _ -> s
      | None, None -> ""
    in

    let required =
      match current_identifier with
      | None -> true
      | Some (`Required, _) -> true
      | Some (`Optional, _) -> false
    in

    let tag_desc = { key; required; tag; attributes; children; transformer } in
    let tags =
      if public then
        env.tags
        @ [
            {
              public_tag_key = key;
              public_tag_typ = tag;
              public_tag_children =
                (match child_env.tags with
                | [] -> None
                | tags -> Some tags);
            };
          ]
      else
        env.tags
    in
    let env = { env with Env.tags } in

    (env, tag_desc)
  in
  let env, tag_desc = transform_tag_desc env tag.tag_desc in
  (env, TagExpression { tag_loc = tag.tag_loc; tag_desc })

and transform_unary_expression env op right =
  let env, right = transform_expression env right in
  (env, UnaryExpression (op, right))

and transform_binary_expression env left op right =
  let env, left = transform_expression env left in
  let env, right = transform_expression env right in
  (env, BinaryExpression (left, op, right))

and transform_for_in env ~index ~iterator ~reverse ~iterable ~body =
  let env, index = Option.fold_map ~init:env ~f:transform_lowercase_id index in
  let env, iterator = transform_lowercase_id env iterator in
  let reverse = reverse in
  let env, iterable = transform_expression env iterable in
  let env, body = transform_expression env body in
  (env, ForInExpression { index; iterator; reverse; iterable; body })

and transform_conditional env ~condition ~consequent ~alternate =
  let env, condition = transform_expression env condition in
  let env, consequent = transform_statement env consequent in
  let env, alternate = Option.fold_map ~init:env ~f:transform_statement alternate in
  (env, ConditionalExpression { condition; consequent; alternate })

and transform_block env statements =
  let env, statements = List.fold_map ~init:env ~f:transform_statement statements in
  (env, BlockExpression statements)

and transform_html_template_node
    env
    ~html_tag_identifier
    ~html_tag_attributes
    ~html_tag_children
    ~html_tag_self_closing =
  let html_tag_identifier = html_tag_identifier in
  let env, html_tag_attributes =
    StringMap.fold_map ~init:env ~f:transform_expression html_tag_attributes
  in
  let env, html_tag_children =
    List.fold_map ~init:env ~f:transform_template_node html_tag_children
  in
  let html_tag_self_closing = html_tag_self_closing in
  ( env,
    HtmlTemplateNode
      {
        html_tag_identifier;
        html_tag_attributes;
        html_tag_children;
        html_tag_self_closing;
      } )

and transform_component_template_node
    env
    ~component_tag_identifier
    ~component_tag_attributes
    ~component_tag_children =
  let env, component_tag_identifier =
    transform_uppercase_id env component_tag_identifier
  in
  let env, component_tag_attributes =
    StringMap.fold_map ~init:env ~f:transform_expression component_tag_attributes
  in
  let env, component_tag_children =
    List.fold_map ~init:env ~f:transform_template_node component_tag_children
  in
  ( env,
    ComponentTemplateNode
      { component_tag_identifier; component_tag_attributes; component_tag_children } )

and transform_expression_template_node env expr =
  let env, expr = transform_expression env expr in
  (env, ExpressionTemplateNode expr)

and transform_text_template_node env s = (env, TextTemplateNode s)

and transform_template_node env (node : Parsetree.template_node) =
  let env, desc =
    match node.template_node_desc with
    | P_HtmlTemplateNode
        {
          html_tag_identifier;
          html_tag_attributes;
          html_tag_children;
          html_tag_self_closing;
        } ->
        transform_html_template_node
          env
          ~html_tag_identifier
          ~html_tag_attributes
          ~html_tag_children
          ~html_tag_self_closing
    | P_ComponentTemplateNode
        { component_tag_identifier; component_tag_attributes; component_tag_children } ->
        transform_component_template_node
          env
          ~component_tag_identifier
          ~component_tag_attributes
          ~component_tag_children
    | P_ExpressionTemplateNode expr -> transform_expression_template_node env expr
    | P_TextTemplateNode s -> transform_text_template_node env s
  in
  (env, { template_node_loc = node.template_node_loc; template_node_desc = desc })

and transform_template env nodes =
  let env, nodes = List.fold_map ~init:env ~f:transform_template_node nodes in
  (env, TemplateExpression nodes)

and transform_expression env (exression : Parsetree.expression) =
  let env, desc =
    match exression.expression_desc with
    | P_Comment comment -> transform_comment env comment
    | P_String templates -> transform_string env templates
    | P_Char c -> transform_char env c
    | P_Int i -> transform_int env i
    | P_Float f -> transform_float env f
    | P_Bool b -> transform_bool env b
    | P_Array array -> transform_array env array
    | P_Record record -> transform_record env record
    | P_ExternalFunction { parameters; name } ->
        transform_external_function env ~loc:exression.expression_loc parameters name
    | P_Function { parameters; body } -> transform_function env parameters body
    | P_FunctionCall { function_definition; arguments } ->
        transform_function_call env function_definition arguments
    | P_UppercaseIdentifierPathExpression path ->
        transform_uppercase_id_path_expression env path
    | P_UppercaseIdentifierExpression id -> transform_uppercase_id_expression env id
    | P_LowercaseIdentifierExpression id -> transform_lowercase_id_expression env id
    | P_TagExpression tag -> transform_tag env tag
    | P_ForInExpression { index; iterator; reverse; iterable; body } ->
        transform_for_in env ~index ~iterator ~reverse ~iterable ~body
    | P_ConditionalExpression { condition; consequent; alternate } ->
        transform_conditional env ~condition ~consequent ~alternate
    | P_BlockExpression statements -> transform_block env statements
    | P_TemplateExpression nodes -> transform_template env nodes
    | P_UnaryExpression (op, expr) -> transform_unary_expression env op expr
    | P_BinaryExpression (left, op, right) ->
        transform_binary_expression env left op right
  in
  (env, { expression_loc = exression.expression_loc; expression_desc = desc })

and transform_comment_stmt env s = (env, CommentStatement s)
and transform_break_stmt env s = (env, BreakStatement s)
and transform_continue_stmt env s = (env, ContinueStatement s)

and transform_use_stmt env id expr =
  let env, id = Option.fold_map ~init:env ~f:transform_uppercase_id id in
  let env, expr = transform_expression env expr in
  (env, UseStatement (id, expr))

and transform_optional_mutable_let env id expr =
  let env = { env with Env.current_identifier = Some (`Optional, id) } in
  let env, id = transform_lowercase_id env id in
  let env, expr = transform_expression env expr in
  let env = { env with Env.current_identifier = None } in
  (env, OptionalMutableLetStatement (id, expr))

and transform_optional_let env id expr =
  let env = { env with Env.current_identifier = Some (`Optional, id) } in
  let env, id = transform_lowercase_id env id in
  let env, expr = transform_expression env expr in
  let env = { env with Env.current_identifier = None } in
  (env, OptionalLetStatement (id, expr))

and transform_mutable_let env id expr =
  let env = { env with Env.current_identifier = Some (`Required, id) } in
  let env, id = transform_lowercase_id env id in
  let env, expr = transform_expression env expr in
  let env = { env with Env.current_identifier = None } in
  (env, MutableLetStatement (id, expr))

and transform_let env id expr =
  let env = { env with Env.current_identifier = Some (`Required, id) } in
  let env, id = transform_lowercase_id env id in
  let env, expr = transform_expression env expr in
  let env = { env with Env.current_identifier = None } in
  (env, LetStatement (id, expr))

and transform_mutation env id expr =
  let env, id = transform_lowercase_id env id in
  let env, expr = transform_expression env expr in
  (env, MutationStatement (id, expr))

and transform_expression_stmt env s =
  let env, expr = transform_expression env s in
  (env, ExpressionStatement expr)

and transform_statement env (statement : Parsetree.statement) =
  let env, desc =
    match statement.statement_desc with
    | P_CommentStatement s -> transform_comment_stmt env s
    | P_BreakStatement s -> transform_break_stmt env s
    | P_ContinueStatement s -> transform_continue_stmt env s
    | P_UseStatement (id, expr) -> transform_use_stmt env id expr
    | P_OptionalMutableLetStatement (id, expr) ->
        transform_optional_mutable_let env id expr
    | P_OptionalLetStatement (id, expr) -> transform_optional_let env id expr
    | P_MutableLetStatement (id, expr) -> transform_mutable_let env id expr
    | P_LetStatement (id, expr) -> transform_let env id expr
    | P_MutationStatement (id, expr) -> transform_mutation env id expr
    | P_ExpressionStatement s -> transform_expression_stmt env s
  in
  (env, { statement_loc = statement.statement_loc; statement_desc = desc })

and transform_component_declaration env (declaration : Parsetree.declaration_desc) =
  let env, declaration_attributes =
    StringMap.fold_map
      ~init:env
      ~f:transform_expression
      declaration.declaration_attributes
  in
  let env, declaration_body = transform_expression env declaration.declaration_body in
  (env, Declaration_Component { declaration_attributes; declaration_body })

and transform_library_declaration env (declaration : Parsetree.declaration_desc) =
  let env, declaration_attributes =
    StringMap.fold_map
      ~init:env
      ~f:transform_expression
      declaration.declaration_attributes
  in
  let env, declaration_body = transform_expression env declaration.declaration_body in
  (env, Declaration_Library { declaration_attributes; declaration_body })

and transform_page_declaration env (declaration : Parsetree.declaration_desc) =
  let env, declaration_attributes =
    StringMap.fold_map
      ~init:env
      ~f:transform_expression
      declaration.declaration_attributes
  in
  let env, declaration_body = transform_expression env declaration.declaration_body in
  (env, Declaration_Page { declaration_attributes; declaration_body })

and transform_store_declaration env (declaration : Parsetree.declaration_desc) =
  let env, declaration_attributes =
    StringMap.fold_map
      ~init:env
      ~f:transform_expression
      declaration.declaration_attributes
  in
  let env, declaration_body = transform_expression env declaration.declaration_body in
  (env, Declaration_Store { declaration_attributes; declaration_body })

and transform_declaration env (declaration : Parsetree.declaration) =
  let env, declaration_type =
    match declaration.declaration_type with
    | P_Declaration_Component desc -> transform_component_declaration env desc
    | P_Declaration_Library desc -> transform_library_declaration env desc
    | P_Declaration_Page desc -> transform_page_declaration env desc
    | P_Declaration_Store desc -> transform_store_declaration env desc
  in
  (env, { declaration_loc = declaration.declaration_loc; declaration_type })

and transform_declarations env declarations =
  StringMap.fold_map ~init:env ~f:transform_declaration declarations
;;

let transform (declarations : Parsetree.t) : Ast.t =
  let env = Env.empty in
  let _env, ast = transform_declarations env declarations in
  ast
;;

let all_tags key declarations =
  let env = Env.empty in
  let declaration = StringMap.find_opt key declarations in
  match declaration with
  | None -> env.tags
  | Some declaration ->
      let env, _ast = transform_declaration env declaration in
      env.tags
;;
