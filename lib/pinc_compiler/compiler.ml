exception TODO

type emitted_instruction = {
  offset : int;
  instruction : Pinc_Bytecode.Instruction.t;
}

type scope = {
  instructions : Buffer.t;
  last_instruction : emitted_instruction;
  previous_instruction : emitted_instruction;
}

type t = {
  constants : Pinc_Bytecode.Value.t Int32.Map.t;
  symbol_table : SymbolTable.t;
  scopes : scope list;
}

let empty_instruction = { offset = 0; instruction = I_Null }

let current_scope t =
  match t.scopes with
  | [] -> assert false
  | scope :: _ -> scope
;;

let current_instructions t =
  let scope = current_scope t in
  scope.instructions
;;

let last_instruction t =
  let scope = current_scope t in
  scope.last_instruction.instruction
;;

let last_instruction_offset t =
  let scope = current_scope t in
  scope.last_instruction.offset
;;

let set_last_instruction t offset instruction =
  match t.scopes with
  | [] -> assert false
  | scope :: scopes ->
      let scope' =
        {
          scope with
          previous_instruction = scope.last_instruction;
          last_instruction = { offset; instruction };
        }
      in
      { t with scopes = scope' :: scopes }
;;

let replace_instruction t offset instruction =
  match t.scopes with
  | [] -> assert false
  | scope :: _ ->
      let current_instructions = Buffer.to_bytes scope.instructions in
      let src = Pinc_Bytecode.Instruction.to_bytes instruction in
      let srcoff = 0 in
      let len = Bytes.length src in
      Bytes.blit src srcoff current_instructions offset len;
      Buffer.truncate scope.instructions 0;
      Buffer.add_bytes scope.instructions current_instructions;
      t
;;

let remove_last_instruction t =
  match t.scopes with
  | [] -> assert false
  | scope :: scopes ->
      Buffer.truncate scope.instructions scope.last_instruction.offset;
      let scope' = { scope with last_instruction = scope.previous_instruction } in
      { t with scopes = scope' :: scopes }
;;

let match_last_instruction t check =
  let scope = current_scope t in
  scope.last_instruction.instruction == check
;;

let add_scope t =
  let scope =
    {
      instructions = Buffer.create 8;
      previous_instruction = empty_instruction;
      last_instruction = empty_instruction;
    }
  in
  {
    t with
    scopes = scope :: t.scopes;
    symbol_table = SymbolTable.add_scope t.symbol_table;
  }
;;

let pop_scope t =
  match t.scopes with
  | [] -> assert false
  | scope :: scopes ->
      ({ t with scopes; symbol_table = SymbolTable.pop_scope t.symbol_table }, scope)
;;

let add_constant =
  let id =
    let id' = ref Int32.minus_one in
    fun () ->
      id' := Int32.succ !id';
      !id'
  in
  fun t constant ->
    let new_id = id () in
    let constants = Int32.Map.add new_id constant t.constants in
    (new_id, { t with constants })
;;

let emit t opcode =
  let scope = current_scope t in
  let offset = Buffer.length scope.instructions in
  Buffer.add_bytes scope.instructions @@ Pinc_Bytecode.Instruction.to_bytes @@ opcode;
  let t = set_last_instruction t offset opcode in
  t
;;

let emit_constant t constant =
  let id, t = add_constant t constant in
  let constant = Pinc_Bytecode.Instruction.I_Constant id in
  emit t constant
;;

let add_symbol t name =
  let symbol_table, address = SymbolTable.define_symbol t.symbol_table ~name in
  ({ t with symbol_table }, address)
;;

let get_symbol ~loc t name =
  let symbol = SymbolTable.resolve_symbol t.symbol_table ~name in
  match symbol with
  | None -> Pinc_Diagnostics.raise_error loc ("Unbound identifier `" ^ name ^ "`")
  | Some symbol -> symbol
;;

let emit_get_symbol t symbol =
  let instruction =
    match SymbolTable.Symbol.scope symbol with
    | SymbolTable.Scope.Global -> Pinc_Bytecode.Instruction.I_Get_Global symbol.address
    | SymbolTable.Scope.Local -> Pinc_Bytecode.Instruction.I_Get_Local symbol.address
  in
  emit t instruction
;;

let emit_set_symbol t symbol =
  let instruction =
    match SymbolTable.Symbol.scope symbol with
    | SymbolTable.Scope.Global ->
        Pinc_Bytecode.Instruction.I_Set_Global (SymbolTable.Symbol.address symbol)
    | SymbolTable.Scope.Local ->
        Pinc_Bytecode.Instruction.I_Set_Local (SymbolTable.Symbol.address symbol)
  in
  let t = emit t instruction in
  t
;;

let compile_string_template t s =
  s
  |> List.fold_left
       (fun (t, index) template ->
         let t =
           match template.Pinc_Types.Ast.string_template_desc with
           | StringInterpolation (Lowercase_Id (name, loc)) ->
               let symbol = get_symbol t ~loc name in
               emit_get_symbol t symbol
           | StringText s -> emit_constant t (Pinc_Bytecode.Value.String s)
         in
         let t =
           if index > 0 then
             emit t @@ Pinc_Bytecode.Instruction.I_Concat
           else
             t
         in
         (t, succ index))
       (t, 0)
  |> fst
;;

let rec compile_expr t (expr : Pinc_Types.Ast.expression) =
  match expr.expression_desc with
  | Void -> t
  | String s -> compile_string_template t s
  | Char c -> emit_constant t (Pinc_Bytecode.Value.Char c)
  | Int i -> emit_constant t (Pinc_Bytecode.Value.Int i)
  | Float f -> emit_constant t (Pinc_Bytecode.Value.Float f)
  | Bool true -> emit t Pinc_Bytecode.Instruction.I_True
  | Bool false -> emit t Pinc_Bytecode.Instruction.I_False
  | LowercaseIdentifierExpression name ->
      let symbol = get_symbol t ~loc:expr.expression_loc name in
      emit_get_symbol t symbol
  | ExternalFunction _ -> raise_notrace TODO
  | UppercaseIdentifierExpression _ -> raise_notrace TODO
  | Array a ->
      let t = Array.fold_left compile_expr t a in
      emit t @@ Pinc_Bytecode.Instruction.I_Array (Int32.of_int @@ Array.length a)
  | Record map ->
      let bindings = StringMap.bindings map in
      let keys, values = List.split bindings in
      let emit_key t key = emit_constant t (Pinc_Bytecode.Value.String key) in
      let t = List.fold_left emit_key t keys in
      let emit_value t (_, expr) = compile_expr t expr in
      let t = List.fold_left emit_value t values in
      let length = Int32.of_int @@ List.length keys in
      let t = emit t @@ Pinc_Bytecode.Instruction.I_Record length in
      t
  | Function { identifier = _; parameters; body } ->
      let t = add_scope t in
      let t =
        List.fold_left
          (fun t (Pinc_Types.Ast.Lowercase_Id (name, _)) -> fst @@ add_symbol t name)
          t
          parameters
      in
      let t =
        match body.expression_desc with
        | Pinc_Types.Ast.BlockExpression _ -> compile_expr t body
        | _ ->
            let t = compile_expr t body in
            emit t @@ Pinc_Bytecode.Instruction.I_Return
      in
      let t =
        if match_last_instruction t I_Pop then (
          let t = remove_last_instruction t in
          emit t @@ Pinc_Bytecode.Instruction.I_Return)
        else
          t
      in
      let t =
        if not @@ match_last_instruction t I_Return then (
          let t = emit t @@ Pinc_Bytecode.Instruction.I_Null in
          emit t @@ Pinc_Bytecode.Instruction.I_Return)
        else
          t
      in
      let num_locals = SymbolTable.length t.symbol_table in
      let num_parameters = List.length parameters in
      let t, scope = pop_scope t in
      let instructions = Buffer.to_bytes scope.instructions in
      let t =
        emit_constant t
        @@ Pinc_Bytecode.Value.Function { num_locals; num_parameters; instructions }
      in
      t
  | FunctionCall { function_definition; arguments } ->
      let t = compile_expr t function_definition in
      let t = List.fold_left compile_expr t arguments in
      let num_arguments = Int32.of_int @@ List.length arguments in
      let t = emit t @@ Pinc_Bytecode.Instruction.I_Call num_arguments in
      t
  | TagExpression _ -> raise_notrace TODO
  | ForInExpression _ -> raise_notrace TODO
  | TemplateExpression node -> compile_template_node t node
  | BlockExpression stmts -> List.fold_left compile_stmt t stmts
  | ConditionalExpression { condition; consequent; alternate } ->
      compile_conditional_expression t ~condition ~consequent ~alternate
  | UnaryExpression (op, right) -> compile_unary_expression t ~op ~right
  | BinaryExpression (left, op, right) -> compile_binary_expression t ~left ~op ~right

and compile_unary_expression t ~op ~right =
  let t = compile_expr t right in
  match op with
  | Pinc_Types.Operators.Unary.MINUS -> emit t Pinc_Bytecode.Instruction.I_Minus
  | Pinc_Types.Operators.Unary.NOT -> emit t Pinc_Bytecode.Instruction.I_Not

and compile_binary_expression t ~left ~op ~right =
  match op with
  | Pinc_Types.Operators.Binary.PLUS ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Add
  | Pinc_Types.Operators.Binary.MINUS ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Sub
  | Pinc_Types.Operators.Binary.DIV ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Div
  | Pinc_Types.Operators.Binary.TIMES ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Mul
  | Pinc_Types.Operators.Binary.MODULO ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Mod
  | Pinc_Types.Operators.Binary.POW ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Pow
  | Pinc_Types.Operators.Binary.EQUAL ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Equal
  | Pinc_Types.Operators.Binary.NOT_EQUAL ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Not_Equal
  | Pinc_Types.Operators.Binary.GREATER ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Greater
  | Pinc_Types.Operators.Binary.GREATER_EQUAL ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Greater_Equal
  | Pinc_Types.Operators.Binary.LESS ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Less
  | Pinc_Types.Operators.Binary.LESS_EQUAL ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Less_Equal
  | Pinc_Types.Operators.Binary.AND ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_And
  | Pinc_Types.Operators.Binary.OR ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Or
  | Pinc_Types.Operators.Binary.CONCAT ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Concat
  | Pinc_Types.Operators.Binary.DOT_ACCESS ->
      let t = compile_expr t left in
      let t =
        match right.expression_desc with
        | Pinc_Types.Ast.LowercaseIdentifierExpression id ->
            emit_constant t (Pinc_Bytecode.Value.String id)
        | _ ->
            (* TODO: We should be able to encode this into the type system *)
            assert false
      in
      emit t Pinc_Bytecode.Instruction.I_Dot_Index
  | Pinc_Types.Operators.Binary.BRACKET_ACCESS ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Index
  | Pinc_Types.Operators.Binary.PIPE -> assert false
  | Pinc_Types.Operators.Binary.ARRAY_ADD -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.MERGE -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.RANGE ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Range
  | Pinc_Types.Operators.Binary.INCLUSIVE_RANGE ->
      let t = compile_expr t left in
      let t = compile_expr t right in
      emit t Pinc_Bytecode.Instruction.I_Range_Inclusive

and compile_conditional_expression t ~condition ~consequent ~alternate =
  (* Condition *)
  let t = compile_expr t condition in

  (* Consequent *)
  (* We create a conditional jump with a temporary address first, because we do not know where we should jump to next. *)
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump_If_False 0xFFFFFFFl) in
  let jump_consequent_offset = last_instruction_offset t in
  let t = compile_expr t consequent in
  let t =
    if match_last_instruction t I_Pop then
      remove_last_instruction t
    else
      t
  in

  (* Alternate *)
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump 0xFFFFFFFl) in
  let jump_alternate_offset = last_instruction_offset t in
  let jump_address = Int32.of_int (Buffer.length @@ current_instructions t) in
  let t =
    replace_instruction t jump_consequent_offset
    @@ Pinc_Bytecode.Instruction.I_Jump_If_False jump_address
  in
  let t =
    match alternate with
    | None -> emit t Pinc_Bytecode.Instruction.I_Null
    | Some alternate ->
        let t = compile_expr t alternate in
        let t =
          if match_last_instruction t I_Pop then
            remove_last_instruction t
          else
            t
        in
        t
  in
  let jump_address = Int32.of_int (Buffer.length @@ current_instructions t) in
  let t =
    replace_instruction t jump_alternate_offset
    @@ Pinc_Bytecode.Instruction.I_Jump jump_address
  in
  t

and compile_stmt t (stmt : Pinc_Types.Ast.statement) =
  match stmt.statement_desc with
  | BreakStatement _ -> raise_notrace TODO
  | ContinueStatement _ -> raise_notrace TODO
  | LetStatement (~is_optional:_, ~is_mutable:_, Lowercase_Id (name, _), expr) ->
      let t = compile_expr t expr in
      let t, symbol = add_symbol t name in
      let t = emit_set_symbol t symbol in
      t
  | MutationStatement (_, _) -> raise_notrace TODO
  | ExpressionStatement e ->
      let t = compile_expr t e in
      emit t Pinc_Bytecode.Instruction.I_Pop

and compile_tag _t (_tag : Pinc_Types.Ast.tag) = raise_notrace TODO

and compile_template_node _t (node : Pinc_Types.Ast.template_node) =
  match node.template_node_desc with
  | TextTemplateNode _ -> raise_notrace TODO
  | FragmentTemplateNode _ -> raise_notrace TODO
  | ExpressionTemplateNode _ -> raise_notrace TODO
  | HtmlTemplateNode _ -> raise_notrace TODO
  | ComponentTemplateNode _ -> raise_notrace TODO
;;

let compile_declaration (decl : Pinc_Types.Ast.declaration) t =
  compile_expr t decl.declaration_body
;;

let compile (ast : Pinc_Types.Ast.t) =
  let scope =
    {
      instructions = Buffer.create 8;
      previous_instruction = empty_instruction;
      last_instruction = empty_instruction;
    }
  in
  let t =
    {
      constants = Int32.Map.empty;
      symbol_table = SymbolTable.make ();
      scopes = [ scope ];
    }
  in
  let t = StringMap.fold (fun _ -> compile_declaration) ast t in
  Pinc_Bytecode.Bytecode.make
    ~instructions:(Buffer.to_bytes @@ current_instructions t)
    ~constants:t.constants
;;
