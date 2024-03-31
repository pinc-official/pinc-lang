let eval ~eval_expression ~state declaration =
  state.Types.Type_State.declarations |> StringMap.find_opt declaration |> function
  | Some
      {
        Pinc_Parser.Ast.declaration_type =
          ( Declaration_Component { declaration_body; _ }
          | Declaration_Library { declaration_body; _ }
          | Declaration_Page { declaration_body; _ }
          | Declaration_Store { declaration_body; _ } );
        _;
      } -> eval_expression ~state declaration_body
  | None ->
      Pinc_Diagnostics.error
        Pinc_Diagnostics.Location.none
        ("Declaration with name `" ^ declaration ^ "` was not found.")
;;
