exception TODO

type emitted_instruction = {
  offset : int;
  instruction : Pinc_Bytecode.Instruction.t;
}

type t = {
  instructions : Buffer.t;
  constants : Pinc_Bytecode.Value.t UInt16.Map.t;
  mutable last_instruction : emitted_instruction;
  mutable previous_instruction : emitted_instruction;
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

let rec compile_expr t (expr : Pinc_Types.Ast.expression) =
  match expr.expression_desc with
  | Void -> t
  | String _ -> raise_notrace TODO
  | Char _ -> raise_notrace TODO
  | Int i -> emit_constant t (Pinc_Bytecode.Value.Int i)
  | Float f -> emit_constant t (Pinc_Bytecode.Value.Float f)
  | Bool true -> emit t Pinc_Bytecode.Instruction.I_True
  | Bool false -> emit t Pinc_Bytecode.Instruction.I_False
  | LowercaseIdentifierExpression _ -> raise_notrace TODO
  | ExternalFunction _ -> raise_notrace TODO
  | UppercaseIdentifierExpression _ -> raise_notrace TODO
  | Array _ -> raise_notrace TODO
  | Record _ -> raise_notrace TODO
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
  let t = compile_expr t left in
  let t = compile_expr t right in
  match op with
  | Pinc_Types.Operators.Binary.PLUS -> emit t Pinc_Bytecode.Instruction.I_Add
  | Pinc_Types.Operators.Binary.MINUS -> emit t Pinc_Bytecode.Instruction.I_Sub
  | Pinc_Types.Operators.Binary.DIV -> emit t Pinc_Bytecode.Instruction.I_Div
  | Pinc_Types.Operators.Binary.TIMES -> emit t Pinc_Bytecode.Instruction.I_Mul
  | Pinc_Types.Operators.Binary.MODULO -> emit t Pinc_Bytecode.Instruction.I_Mod
  | Pinc_Types.Operators.Binary.POW -> emit t Pinc_Bytecode.Instruction.I_Pow
  | Pinc_Types.Operators.Binary.EQUAL -> emit t Pinc_Bytecode.Instruction.I_Equal
  | Pinc_Types.Operators.Binary.NOT_EQUAL -> emit t Pinc_Bytecode.Instruction.I_Not_Equal
  | Pinc_Types.Operators.Binary.GREATER -> emit t Pinc_Bytecode.Instruction.I_Greater
  | Pinc_Types.Operators.Binary.GREATER_EQUAL ->
      emit t Pinc_Bytecode.Instruction.I_Greater_Equal
  | Pinc_Types.Operators.Binary.LESS -> emit t Pinc_Bytecode.Instruction.I_Less
  | Pinc_Types.Operators.Binary.LESS_EQUAL ->
      emit t Pinc_Bytecode.Instruction.I_Less_Equal
  | Pinc_Types.Operators.Binary.AND -> emit t Pinc_Bytecode.Instruction.I_And
  | Pinc_Types.Operators.Binary.OR -> emit t Pinc_Bytecode.Instruction.I_Or
  | Pinc_Types.Operators.Binary.CONCAT -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.DOT_ACCESS -> raise_notrace TODO
  | Pinc_Types.Operators.Binary.BRACKET_ACCESS -> raise_notrace TODO
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
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump_If_False (UInt16.make 0xFFFF)) in
  let jump_consequent_offset = t.last_instruction.offset in
  let t = compile_expr t consequent in
  let t = remove_last_pop t in

  (* Alternate *)
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump (UInt16.make 0xFFFF)) in
  let jump_alternate_offset = t.last_instruction.offset in
  let jump_address = UInt16.make (Buffer.length t.instructions) in
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
  let jump_address = UInt16.make (Buffer.length t.instructions) in
  let t =
    replace_instruction t jump_alternate_offset
    @@ Pinc_Bytecode.Instruction.I_Jump jump_address
  in
  t

and compile_stmt t (stmt : Pinc_Types.Ast.statement) =
  match stmt.statement_desc with
  | BreakStatement _ -> raise_notrace TODO
  | ContinueStatement _ -> raise_notrace TODO
  | LetStatement (_, _) -> raise_notrace TODO
  | MutableLetStatement (_, _) -> raise_notrace TODO
  | OptionalLetStatement (_, _) -> raise_notrace TODO
  | OptionalMutableLetStatement (_, _) -> raise_notrace TODO
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
      constants = UInt16.Map.empty;
      previous_instruction = empty_instruction;
      last_instruction = empty_instruction;
    }
  in
  let t = StringMap.fold (fun _ -> compile_declaration) ast t in
  Pinc_Bytecode.Bytecode.make
    ~instructions:(Buffer.to_bytes t.instructions)
    ~constants:t.constants
;;
