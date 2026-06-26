exception TODO

type emitted_instruction = {
  offset : int;
  instruction : Pinc_Bytecode.Instruction.t;
}

type t = {
  instructions : Buffer.t;
  constants : Pinc_Bytecode.Value.t Int32.Map.t;
  mutable last_instruction : emitted_instruction;
  mutable previous_instruction : emitted_instruction;
  symbol_table : SymbolTable.t;
}

let set_last_instruction t offset instruction =
  t.previous_instruction <- t.last_instruction;
  t.last_instruction <- { offset; instruction }
;;

let replace_instruction t offset instruction =
  let current_instructions = Buffer.to_bytes t.instructions in
  let src = Pinc_Bytecode.Instruction.to_bytes instruction in
  let srcoff = 0 in
  let len = Bytes.length src in
  Bytes.blit src srcoff current_instructions offset len;
  Buffer.truncate t.instructions 0;
  Buffer.add_bytes t.instructions current_instructions;
  t
;;

let remove_last_instruction t =
  Buffer.truncate t.instructions t.last_instruction.offset;
  t.last_instruction <- t.previous_instruction;
  t
;;

let remove_last_pop t =
  match t.last_instruction.instruction with
  | I_Pop -> remove_last_instruction t
  | _ -> t
;;

let add_constant =
  let id =
    let id' = ref Int32.zero in
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
  let offset = Buffer.length t.instructions in
  Buffer.add_bytes t.instructions @@ Pinc_Bytecode.Instruction.to_bytes @@ opcode;
  set_last_instruction t offset opcode;
  t
;;

let emit_constant t constant =
  let id, t = add_constant t constant in
  let constant = Pinc_Bytecode.Instruction.I_Constant id in
  emit t constant
;;

let add_symbol t name =
  let symbol_table = SymbolTable.define_symbol t.symbol_table ~name in
  ({ t with symbol_table }, SymbolTable.length symbol_table)
;;

let get_symbol ~loc t name =
  let symbol = SymbolTable.resolve_symbol t.symbol_table ~name in
  match symbol with
  | None -> Pinc_Diagnostics.raise_error loc ("Unbound identifier `" ^ name ^ "`")
  | Some symbol -> symbol
;;

let compile_string_template t s =
  s
  |> List.fold_left
       (fun (t, index) template ->
         let t =
           match template.Pinc_Types.Ast.string_template_desc with
           | StringInterpolation (Lowercase_Id (name, loc)) ->
               let symbol = get_symbol t ~loc name in
               emit t @@ Pinc_Bytecode.Instruction.I_Get_Global symbol.address
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
  | Char _ -> raise_notrace TODO
  | Int i -> emit_constant t (Pinc_Bytecode.Value.Int i)
  | Float f -> emit_constant t (Pinc_Bytecode.Value.Float f)
  | Bool true -> emit t Pinc_Bytecode.Instruction.I_True
  | Bool false -> emit t Pinc_Bytecode.Instruction.I_False
  | LowercaseIdentifierExpression name ->
      let symbol = get_symbol ~loc:expr.expression_loc t name in
      emit t @@ Pinc_Bytecode.Instruction.I_Get_Global symbol.address
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
  | Function _ -> raise_notrace TODO
  | FunctionCall _ -> raise_notrace TODO
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
  | Pinc_Types.Operators.Binary.FUNCTION_CALL -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.PIPE -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.ARRAY_ADD -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.MERGE -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.RANGE -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.INCLUSIVE_RANGE -> raise_notrace TODO

and compile_conditional_expression t ~condition ~consequent ~alternate =
  (* Condition *)
  let t = compile_expr t condition in

  (* Consequent *)
  (* We create a conditional jump with a temporary address first, because we do not know where we should jump to next. *)
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump_If_False 0xFFFFFFFl) in
  let jump_consequent_offset = t.last_instruction.offset in
  let t = compile_expr t consequent in
  let t = remove_last_pop t in

  (* Alternate *)
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump 0xFFFFFFFl) in
  let jump_alternate_offset = t.last_instruction.offset in
  let jump_address = Int32.of_int (Buffer.length t.instructions) in
  let t =
    replace_instruction t jump_consequent_offset
    @@ Pinc_Bytecode.Instruction.I_Jump_If_False jump_address
  in
  let t =
    match alternate with
    | None -> emit t Pinc_Bytecode.Instruction.I_Null
    | Some alternate ->
        let t = compile_expr t alternate in
        let t = remove_last_pop t in
        t
  in
  let jump_address = Int32.of_int (Buffer.length t.instructions) in
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
      let t, addr = add_symbol t name in
      let t = emit t (Pinc_Bytecode.Instruction.I_Set_Global addr) in
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
  let empty_instruction = { offset = 0; instruction = I_Null } in
  let t =
    {
      instructions = Buffer.create 8;
      constants = Int32.Map.empty;
      previous_instruction = empty_instruction;
      last_instruction = empty_instruction;
      symbol_table = SymbolTable.make ();
    }
  in
  let t = StringMap.fold (fun _ -> compile_declaration) ast t in
  Pinc_Bytecode.Bytecode.make
    ~instructions:(Buffer.to_bytes t.instructions)
    ~constants:t.constants
;;
