module Ast = Pinc_Typer.Typed_tree

let eval ~eval_expression ~state declaration =
  state.Types.Type_State.declarations |> StringMap.find_opt declaration |> function
  | Some
      {
        Ast.declaration_kind =
          ( T_Declaration_Component { declaration_body; _ }
          | T_Declaration_Library { declaration_body; _ }
          | T_Declaration_Page { declaration_body; _ }
          | T_Declaration_Store { declaration_body; _ } );
        _;
      } -> eval_expression ~state declaration_body
  | None ->
      Pinc_Diagnostics.raise_error
        Pinc_Diagnostics.Location.none
        ("Declaration with name `" ^ declaration ^ "` was not found.")
;;
