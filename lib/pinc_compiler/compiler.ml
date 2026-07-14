exception TODO

type emitted_instruction = {
  offset : int;
  instruction : Pinc_Bytecode.Instruction.t;
}

type frame = {
  instructions : Pinc_Bytecode.Instruction.t Dynarray.t;
  last_instruction : emitted_instruction;
  previous_instruction : emitted_instruction;
  loop_level : int;
  break_instructions : (offset:int * n:int) list;
  continue_instructions : (offset:int * n:int) list;
}

type t = {
  constants : Pinc_Bytecode.Value.t Dynarray.t;
  symbol_table : SymbolTable.t;
  frames : frame list;
}

let empty_instruction = { offset = 0; instruction = I_Null }

let current_frame t =
  match t.frames with
  | [] -> assert false
  | scope :: _ -> scope
;;

let current_instructions t =
  let frame = current_frame t in
  frame.instructions
;;

let last_instruction t =
  let frame = current_frame t in
  frame.last_instruction.instruction
;;

let last_instruction_offset t =
  let frame = current_frame t in
  frame.last_instruction.offset
;;

let set_last_instruction t offset instruction =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      let frame' =
        {
          frame with
          previous_instruction = frame.last_instruction;
          last_instruction = { offset; instruction };
        }
      in
      { t with frames = frame' :: frames }
;;

let replace_instruction t offset instruction =
  match t.frames with
  | [] -> assert false
  | frame :: _ ->
      Dynarray.set frame.instructions offset instruction;
      t
;;

let remove_last_instruction t =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      Dynarray.remove_last frame.instructions;
      let frame' = { frame with last_instruction = frame.previous_instruction } in
      { t with frames = frame' :: frames }
;;

let match_last_instruction t check =
  let frame = current_frame t in
  frame.last_instruction.instruction == check
;;

let enter_loop t =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      let frame' = { frame with loop_level = succ frame.loop_level } in
      { t with frames = frame' :: frames }
;;

let exit_loop t =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      let frame' = { frame with loop_level = pred frame.loop_level } in
      { t with frames = frame' :: frames }
;;

let loop_level t =
  match t.frames with
  | [] -> assert false
  | frame :: _ -> frame.loop_level
;;

let add_break_instruction t ~offset ~n =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      let frame' =
        { frame with break_instructions = (~offset, ~n) :: frame.break_instructions }
      in
      { t with frames = frame' :: frames }
;;

let add_continue_instruction t ~offset ~n =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      let frame' =
        {
          frame with
          continue_instructions = (~offset, ~n) :: frame.continue_instructions;
        }
      in
      { t with frames = frame' :: frames }
;;

let fixup_continue_break_address t ~continue_address ~break_address =
  let loop_level = loop_level t in
  let update_instruction frame address instruction =
    let ~offset, ~n = instruction in
    if loop_level = n then (
      Dynarray.set frame.instructions offset (Pinc_Bytecode.Instruction.I_Jump address);
      None)
    else
      Some (~offset, ~n)
  in
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      let continue_instructions =
        let fn = update_instruction frame continue_address in
        List.filter_map fn frame.continue_instructions
      in
      let break_instructions =
        let fn = update_instruction frame break_address in
        List.filter_map fn frame.break_instructions
      in
      let frame' = { frame with continue_instructions; break_instructions } in
      { t with frames = frame' :: frames }
;;

let add_frame t =
  let frame =
    {
      instructions = Dynarray.create ();
      previous_instruction = empty_instruction;
      last_instruction = empty_instruction;
      loop_level = 0;
      break_instructions = [];
      continue_instructions = [];
    }
  in
  {
    t with
    frames = frame :: t.frames;
    symbol_table = SymbolTable.add_frame t.symbol_table;
  }
;;

let pop_frame t =
  match t.frames with
  | [] -> assert false
  | frame :: frames ->
      ({ t with frames; symbol_table = SymbolTable.pop_frame t.symbol_table }, frame)
;;

let add_scope t = { t with symbol_table = SymbolTable.add_scope t.symbol_table }
let pop_scope t = { t with symbol_table = SymbolTable.pop_scope t.symbol_table }

let add_constant t constant =
  let () = Dynarray.add_last t.constants constant in
  let id = Dynarray.length t.constants - 1 in
  (id, t)
;;

let emit t opcode =
  let frame = current_frame t in
  let offset = Dynarray.length frame.instructions in
  Dynarray.add_last frame.instructions opcode;
  let t = set_last_instruction t offset opcode in
  t
;;

let emit_constant t constant =
  let id, t = add_constant t constant in
  let constant = Pinc_Bytecode.Instruction.I_Constant id in
  emit t constant
;;

let define_symbol t name ~is_mutable =
  let symbol_table, address =
    SymbolTable.define_symbol t.symbol_table ~name ~is_mutable
  in
  ({ t with symbol_table }, address)
;;

let get_symbol ~loc t name =
  let symbol = SymbolTable.resolve_symbol t.symbol_table ~name in
  match symbol with
  | _, None -> Pinc_Diagnostics.raise_error loc ("Unbound identifier `" ^ name ^ "`")
  | symbol_table, Some symbol -> ({ t with symbol_table }, symbol)
;;

let emit_get_symbol t symbol =
  let instruction =
    match SymbolTable.Symbol.kind symbol with
    | SymbolTable.Kind.Global -> Pinc_Bytecode.Instruction.I_Get_Global symbol.address
    | SymbolTable.Kind.Local -> Pinc_Bytecode.Instruction.I_Get_Local symbol.address
    | SymbolTable.Kind.Free -> Pinc_Bytecode.Instruction.I_Get_Free symbol.address
    | SymbolTable.Kind.Function -> Pinc_Bytecode.Instruction.I_Current_Closure
  in
  emit t instruction
;;

let emit_set_symbol t symbol =
  let instruction =
    match SymbolTable.Symbol.kind symbol with
    | SymbolTable.Kind.Global ->
        Pinc_Bytecode.Instruction.I_Set_Global (SymbolTable.Symbol.address symbol)
    | SymbolTable.Kind.Local ->
        Pinc_Bytecode.Instruction.I_Set_Local (SymbolTable.Symbol.address symbol)
    | SymbolTable.Kind.Free -> assert false
    | SymbolTable.Kind.Function -> assert false
  in
  let t = emit t instruction in
  t
;;

let rec compile_expr t (expr : Pinc_Types.Ast.expression) =
  let loc = expr.expression_loc in
  match expr.expression_desc with
  | Void -> t
  | String s -> compile_string_template t s
  | Char c -> compile_char t c
  | Int i -> compile_int t i
  | Float f -> compile_float t f
  | Bool b -> compile_bool t b
  | LowercaseIdentifierExpression name -> compile_lowercase_identifier t name ~loc
  | ExternalFunction { identifier = _; parameters; name } ->
      compile_external_function t ~loc ~name ~parameters
  | UppercaseIdentifierExpression _ -> raise_notrace TODO
  | Array a -> compile_array t a
  | Record map -> compile_record t map
  | Function { identifier; parameters; body } ->
      compile_function t ~identifier ~parameters ~body
  | FunctionCall { function_definition; arguments } ->
      compile_function_call t ~fn:function_definition ~arguments
  | TagExpression _ -> raise_notrace TODO
  | ForInExpression { index; iterator; reverse; iterable; body } ->
      compile_loop_expression t ~index ~iterator ~reverse ~iterable ~body
  | TemplateExpression node -> compile_template_node t node
  | BlockExpression stmts -> compile_block_expression t stmts
  | ConditionalExpression { condition; consequent; alternate } ->
      compile_conditional_expression t ~condition ~consequent ~alternate
  | UnaryExpression (op, right) -> compile_unary_expression t ~op ~right
  | BinaryExpression (left, op, right) -> compile_binary_expression t ~left ~op ~right

and compile_string_template t s =
  match s with
  | [] -> emit_constant t (Pinc_Bytecode.Value.String "")
  | s ->
      fst
      @@ List.fold_left
           (fun (t, index) template ->
             let t =
               match template.Pinc_Types.Ast.string_template_desc with
               | StringInterpolation (Lowercase_Id (name, loc)) ->
                   let t, symbol = get_symbol t ~loc name in
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
           s

and compile_char t c = emit_constant t (Pinc_Bytecode.Value.Char c)
and compile_int t i = emit_constant t (Pinc_Bytecode.Value.Int i)
and compile_float t f = emit_constant t (Pinc_Bytecode.Value.Float f)

and compile_bool t b =
  if b then
    emit t Pinc_Bytecode.Instruction.I_True
  else
    emit t Pinc_Bytecode.Instruction.I_False

and compile_lowercase_identifier t ~loc id =
  let t, symbol = get_symbol t ~loc id in
  emit_get_symbol t symbol

and compile_external_function t ~loc ~name ~parameters =
  let index =
    Pinc_Bytecode.Externals.find_index name |> function
    | None -> Pinc_Diagnostics.raise_error loc ("Unbound external function `" ^ name ^ "`")
    | Some i -> i
  in
  let expected_parameters = Pinc_Bytecode.Externals.expected_parameters index in
  let parameters = List.length parameters in
  let t =
    if not @@ Int.equal expected_parameters parameters then
      Pinc_Diagnostics.raise_error
        loc
        ("External function %%"
        ^ name
        ^ "%% expected "
        ^ string_of_int expected_parameters
        ^ " parameters, but got "
        ^ string_of_int parameters)
    else
      emit t @@ Pinc_Bytecode.Instruction.I_Get_Builtin index
  in
  t

and compile_array t array =
  let t = Array.fold_left compile_expr t array in
  let t = emit_constant t @@ Pinc_Bytecode.Value.Int (Array.length array) in
  emit t @@ Pinc_Bytecode.Instruction.I_Array

and compile_record t record =
  let bindings = StringMap.bindings record in
  let keys, values = List.split bindings in
  let emit_key t key = emit_constant t (Pinc_Bytecode.Value.String key) in
  let t = List.fold_left emit_key t keys in
  let emit_value t (_, expr) = compile_expr t expr in
  let t = List.fold_left emit_value t values in
  let length = List.length keys in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Record length in
  t

and compile_block_expression t stmts =
  let t = add_scope t in
  let t = List.fold_left compile_stmt t stmts in
  let t =
    if match_last_instruction t I_Pop then
      remove_last_instruction t
    else
      t
  in
  let t = pop_scope t in
  t

and compile_function t ~identifier ~parameters ~body =
  let t = add_frame t in
  let t =
    match identifier with
    | None -> t
    | Some (Pinc_Types.Ast.Lowercase_Id (name, _)) ->
        let symbol_table, _symbol =
          SymbolTable.define_function_symbol t.symbol_table ~name
        in
        { t with symbol_table }
  in
  let t =
    List.fold_left
      (fun t (Pinc_Types.Ast.Lowercase_Id (name, _)) ->
        fst @@ define_symbol t name ~is_mutable:false)
      t
      parameters
  in
  let t =
    match body.expression_desc with
    | Pinc_Types.Ast.BlockExpression stmts -> List.fold_left compile_stmt t stmts
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
  let free_variables = SymbolTable.free_variables t.symbol_table in
  let num_locals = SymbolTable.length t.symbol_table in
  let t, frame = pop_frame t in
  let t = List.fold_left emit_get_symbol t free_variables in

  let num_free_variables = List.length free_variables in
  let num_parameters = List.length parameters in
  let instructions = Dynarray.to_array @@ frame.instructions in
  let fn_addr, t =
    add_constant t
    @@ Pinc_Bytecode.Value.Function
         { fn_addr = -1; num_locals; num_parameters; instructions }
  in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Closure (fn_addr, num_free_variables) in
  t

and compile_function_call t ~fn ~arguments =
  let t = compile_expr t fn in
  let t = List.fold_left compile_expr t arguments in
  let num_arguments = List.length arguments in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Call num_arguments in
  t

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
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump_If_False 0xFFFFFFF) in
  let jump_consequent_offset = last_instruction_offset t in
  let t = compile_expr t consequent in
  let t =
    if match_last_instruction t I_Pop then
      remove_last_instruction t
    else
      t
  in

  (* Alternate *)
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump 0xFFFFFFF) in
  let jump_alternate_offset = last_instruction_offset t in
  let jump_address = Dynarray.length @@ current_instructions t in
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
  let jump_address = Dynarray.length @@ current_instructions t in
  let t =
    replace_instruction t jump_alternate_offset
    @@ Pinc_Bytecode.Instruction.I_Jump jump_address
  in
  t

and compile_loop_expression t ~index ~iterator ~reverse:_ ~iterable ~body =
  let (Lowercase_Id (iterator, _)) = iterator in
  let t = add_scope t in
  let t = enter_loop t in
  let t, iterator_symbol = define_symbol t iterator ~is_mutable:false in
  let t, length_symbol = define_symbol t ".length" ~is_mutable:false in
  (* Index *)
  let index_identifier =
    match index with
    | Some (Lowercase_Id (name, _)) -> name
    | None -> ".index"
  in
  let t = emit_constant t @@ Pinc_Bytecode.Value.Int 0 in
  let t, index_symbol = define_symbol t index_identifier ~is_mutable:false in
  let t = emit_set_symbol t index_symbol in
  (* Iterations *)
  let t = emit_constant t @@ Pinc_Bytecode.Value.Int 0 in
  let t, n_iterations_symbol = define_symbol t ".n_iterations" ~is_mutable:false in
  let t = emit_set_symbol t n_iterations_symbol in
  (* Iterable *)
  let t = compile_expr t iterable in
  let t, iterable_symbol = define_symbol t ".iterable" ~is_mutable:false in
  let t = emit_set_symbol t iterable_symbol in
  let t = emit_get_symbol t iterable_symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Length in
  let t = emit_set_symbol t length_symbol in
  (* Set Iterator *)
  let loop_start_address = Dynarray.length @@ current_instructions t in
  let t = emit_get_symbol t iterable_symbol in
  let t = emit_get_symbol t index_symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Index in
  let t = emit_set_symbol t iterator_symbol in
  (* Run body *)
  let t = compile_expr t body in
  (* Increment and set number of iterations *)
  let t = emit_constant t @@ Pinc_Bytecode.Value.Int 1 in
  let t = emit_get_symbol t n_iterations_symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Add in
  let t = emit_set_symbol t n_iterations_symbol in
  let after_body_address = Dynarray.length @@ current_instructions t in
  (* Increment and set new index *)
  let t = emit_constant t @@ Pinc_Bytecode.Value.Int 1 in
  let t = emit_get_symbol t index_symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Add in
  let t = emit_set_symbol t index_symbol in
  (* Check array length and jump to loop start *)
  let t = emit_get_symbol t index_symbol in
  let t = emit_get_symbol t length_symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Greater_Equal in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Jump_If_False loop_start_address in
  (* Fixup continue and break statements *)
  let loop_end_address = Dynarray.length @@ current_instructions t in
  (* Create array with values left on stack *)
  let t = emit_get_symbol t n_iterations_symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Array in
  let t =
    fixup_continue_break_address
      t
      ~continue_address:after_body_address
      ~break_address:loop_end_address
  in
  let t = exit_loop t in
  let t = pop_scope t in
  t

and compile_stmt t (stmt : Pinc_Types.Ast.statement) =
  match stmt.statement_desc with
  | BreakStatement -> compile_break_stmt t
  | ContinueStatement -> compile_continue_stmt t
  | LetGroupStatement definitions -> compile_let_group_stmt t definitions
  | LetStatement definition -> compile_let_stmt ~predefined:[] t definition
  | MutationStatement (id, expr) -> compile_mutation_stmt t ~id ~expr
  | ExpressionStatement expr -> compile_expression_stmt t ~expr

and compile_break_stmt t =
  let loop_level = loop_level t in
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump 0xFFFFFFF) in
  let offset = last_instruction_offset t in
  add_break_instruction t ~offset ~n:loop_level

and compile_continue_stmt t =
  let loop_level = loop_level t in
  let t = emit t (Pinc_Bytecode.Instruction.I_Jump 0xFFFFFFF) in
  let offset = last_instruction_offset t in
  add_continue_instruction t ~offset ~n:loop_level

and compile_let_stmt ~predefined t definition =
  let ~is_optional:_, ~is_mutable, Pinc_Types.Ast.Lowercase_Id (name, _), body =
    definition
  in
  let t = compile_expr t body in
  let t, symbol =
    match List.assoc_opt name predefined with
    | None -> define_symbol t name ~is_mutable
    | Some symbol -> (t, symbol)
  in
  let t = emit_set_symbol t symbol in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Null in
  let t = emit t Pinc_Bytecode.Instruction.I_Pop in
  t

and compile_expression_stmt t ~expr =
  let t = compile_expr t expr in
  emit t Pinc_Bytecode.Instruction.I_Pop

and compile_mutation_stmt t ~id ~expr =
  let (Lowercase_Id (name, loc)) = id in
  let t, symbol = get_symbol ~loc t name in
  let t =
    match SymbolTable.Symbol.is_mutable symbol with
    | true ->
        let t = compile_expr t expr in
        emit_set_symbol t symbol
    | false ->
        Pinc_Diagnostics.raise_error
          loc
          ("Trying to update a non mutable variable `" ^ name ^ "`.")
  in
  let t = emit t @@ Pinc_Bytecode.Instruction.I_Null in
  let t = emit t Pinc_Bytecode.Instruction.I_Pop in
  t

and compile_let_group_stmt t definitions =
  let t, predefined =
    List.fold_left
      (fun (t, symbols) definition ->
        let ~is_optional:_, ~is_mutable, Pinc_Types.Ast.Lowercase_Id (name, _), _ =
          definition
        in
        let t, symbol = define_symbol t name ~is_mutable in
        (t, (name, symbol) :: symbols))
      (t, [])
      definitions
  in
  List.fold_left (compile_let_stmt ~predefined) t definitions

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
  let frames =
    [
      {
        instructions = Dynarray.create ();
        previous_instruction = empty_instruction;
        last_instruction = empty_instruction;
        loop_level = 0;
        break_instructions = [];
        continue_instructions = [];
      };
    ]
  in
  let constants = Dynarray.create () in
  let () = Dynarray.ensure_capacity constants 2048 in
  let symbol_table = SymbolTable.make () in
  let t = { constants; symbol_table; frames } in
  let t = StringMap.fold (fun _ -> compile_declaration) ast t in

  Pinc_Bytecode.Bytecode.serialize
  @@ Pinc_Bytecode.Bytecode.make
       ~instructions:(Dynarray.to_array @@ current_instructions t)
       ~constants:t.constants
;;
