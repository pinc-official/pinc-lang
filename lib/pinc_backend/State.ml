open Types
include Types.Type_State

let make
    ?(context = StringMap.empty)
    ?(tag_path = [])
    ~tag_meta
    ~tag_data_provider
    ~root_tag_data_provider
    ~tag_meta_provider
    ~root_tag_meta_provider
    ~mode
    declarations =
  {
    binding_identifier = None;
    declarations;
    output = { value_desc = Null; value_loc = Pinc_Diagnostics.Location.none };
    environment = { scope = []; use_scope = StringMap.empty };
    tag_data_provider;
    root_tag_data_provider;
    tag_meta_provider;
    root_tag_meta_provider;
    tag_meta;
    tag_path;
    context;
    mode;
  }
;;

let add_scope t =
  {
    t with
    environment = { t.environment with scope = StringMap.empty :: t.environment.scope };
  }
;;

let remove_scope t =
  let new_scope =
    match t.environment.scope with
    | [] -> []
    | [ hd ] -> [ hd ]
    | _hd :: tl -> tl
  in
  { t with environment = { t.environment with scope = new_scope } }
;;

let add_value_to_scope ~ident ~value ~is_optional ~is_mutable t =
  let update_scope t =
    match t.environment.scope with
    | [] -> assert false
    | scope :: rest ->
        StringMap.add ident { is_mutable; is_optional; value } scope :: rest
  in
  let environment = { t.environment with scope = update_scope t } in
  { t with environment }
;;

let add_value_to_use_scope ~ident ~value t =
  let use_scope = StringMap.add ident value t.environment.use_scope in
  let environment = { t.environment with use_scope } in
  { t with environment }
;;

let update_value_in_scope ~ident ~value t =
  let updated = ref false in
  let rec update_scope state =
    state.environment.scope
    |> List.map
         (StringMap.mapi (fun key binding ->
              match binding with
              | binding when key = ident && binding.is_mutable -> { binding with value }
              | {
                  value =
                    { value_desc = Function { state = fn_state; parameters; exec }; _ };
                  _;
                } as binding
                when not !updated ->
                  updated := true;
                  fn_state.environment.scope <- update_scope fn_state;
                  {
                    binding with
                    value =
                      {
                        value with
                        value_desc = Function { state = fn_state; parameters; exec };
                      };
                  }
              | v -> v))
  in
  let new_scope = update_scope t in
  t.environment.scope <- new_scope
;;

let add_value_to_function_scopes ~ident ~value ~is_optional ~is_mutable t =
  let update_scope state =
    List.map
      (StringMap.map (function
          | { value = { value_desc = Function { state; parameters; exec }; _ }; _ } as
            binding ->
              let new_state =
                add_value_to_scope ~ident ~value ~is_optional ~is_mutable state
              in
              {
                binding with
                value =
                  {
                    value with
                    value_desc = Function { state = new_state; parameters; exec };
                  };
              }
          | v -> v))
      state.environment.scope
  in
  t.environment.scope <- update_scope t
;;

let get_value_from_scope ~ident t =
  t.environment.scope |> List.find_map (StringMap.find_opt ident)
;;

let get_output t = t.output
let add_output ~output t = { t with output }
let get_bindings t = t.environment.scope |> List.hd
let get_used_values t = t.environment.use_scope

let add_tag_meta ~meta key t =
  match meta with
  | None -> t
  | Some meta ->
      let merged =
        match t.tag_meta |> List.mem_assoc key with
        | false -> t.tag_meta @ [ (key, meta) ]
        | true -> (
            t.tag_meta
            |> List.map @@ function
               | k, v when k = key -> (k, Helpers.TagMeta.merge v meta)
               | v -> v)
      in
      { t with tag_meta = merged }
;;
