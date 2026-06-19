type t = {
  instructions : Buffer.t;
  constants : Pinc_Bytecode.Value.t UInt16.Map.t;
}

let add_constant =
  let id =
    let id' = ref (UInt16.make 0) in
    fun () ->
      UInt16.incr id';
      !id'
  in
  fun t constant ->
    let new_id = id () in
    let constants = UInt16.Map.add new_id constant t.constants in
    (new_id, { t with constants })
;;

let emit_constant t constant =
  let id, t = add_constant t constant in
  let constant =
    Pinc_Bytecode.Instruction.to_bytes @@ Pinc_Bytecode.Instruction.I_Constant id
  in
  Buffer.add_bytes t.instructions constant;
  t
;;

let emit t opcode =
  Buffer.add_bytes t.instructions @@ Pinc_Bytecode.Instruction.to_bytes @@ opcode;
  t
;;

let rec compile_expr t (expr : Pinc_Types.Ast.expression) =
  match expr.expression_desc with
  | Void -> t
  | String _ -> t
  | Char _ -> t
  | Int i -> emit_constant t (Pinc_Bytecode.Value.Int i)
  | Float _ -> t
  | Bool _ -> t
  | LowercaseIdentifierExpression _ -> t
  | ExternalFunction _ -> t
  | UppercaseIdentifierExpression _ -> t
  | Array _ -> t
  | Record _ -> t
  | Function _ -> t
  | FunctionCall _ -> t
  | TagExpression _ -> t
  | ForInExpression _ -> t
  | TemplateExpression node -> compile_template_node t node
  | BlockExpression stmts -> List.fold_left compile_stmt t stmts
  | ConditionalExpression _ -> t
  | UnaryExpression (_, e) -> compile_expr t e
  | BinaryExpression (l, op, r) ->
      let t = compile_expr t l in
      let t = compile_expr t r in
      let t =
        match op with
        | Pinc_Types.Operators.Binary.PLUS -> emit t Pinc_Bytecode.Instruction.I_Add
        | _ -> assert false (* TODO *)
      in
      t

and compile_stmt t (stmt : Pinc_Types.Ast.statement) =
  match stmt.statement_desc with
  | BreakStatement _ | ContinueStatement _ -> t
  | LetStatement (_, e)
  | MutableLetStatement (_, e)
  | OptionalLetStatement (_, e)
  | OptionalMutableLetStatement (_, e)
  | MutationStatement (_, e)
  | ExpressionStatement e ->
      let t = compile_expr t e in
      emit t Pinc_Bytecode.Instruction.I_Pop

and compile_tag t (_tag : Pinc_Types.Ast.tag) = t

and compile_template_node t (node : Pinc_Types.Ast.template_node) =
  match node.template_node_desc with
  | TextTemplateNode _ -> t
  | FragmentTemplateNode _ -> t
  | ExpressionTemplateNode e -> compile_expr t e
  | HtmlTemplateNode _ -> t
  | ComponentTemplateNode _ -> t
;;

let compile_declaration (decl : Pinc_Types.Ast.declaration) t =
  compile_expr t decl.declaration_body
;;

let compile (ast : Pinc_Types.Ast.t) =
  let t = { instructions = Buffer.create 8; constants = UInt16.Map.empty } in
  let t = StringMap.fold (fun _ -> compile_declaration) ast t in
  Pinc_Bytecode.Bytecode.make
    ~instructions:(Buffer.to_bytes t.instructions)
    ~constants:t.constants
;;
