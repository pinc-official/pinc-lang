let eval ~eval_expression ~state declaration =
  state.Types.Type_State.declarations |> StringMap.find_opt declaration |> function
  | Some { Pinc_Parser.Ast.declaration_body; _ } ->
      eval_expression ~state declaration_body
  | None ->
      Pinc_Diagnostics.raise_error
        Pinc_Diagnostics.Location.none
        ("Declaration with name `" ^ declaration ^ "` was not found.")
;;
