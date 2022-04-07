module rec Value : sig
  type definition_info = string * [ `Exists | `DoesntExist ] * [ `Negated | `NotNegated ]

  type t =
    [ `Null
    | `String of string
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Array of t array
    | `Record of t StringMap.t
    | `Function of function_info
    | `DefinitionInfo of definition_info
    | `TagInfo of Tag.info
    | `TemplateNode of
      [ `Component of unit -> t | `Html ] * string * t StringMap.t * t list * bool
    ]

  and function_info =
    { parameters : string list
    ; state : State.t
    ; exec : arguments:t StringMap.t -> state:State.t -> unit -> t
    }

  val null : unit -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_float : float -> t
  val of_list : t list -> t
  val of_string_map : t StringMap.t -> t

  val make_component
    :  render:(unit -> t)
    -> tag:string
    -> attributes:t StringMap.t
    -> children:t list
    -> t

  val to_string : t -> string
  val is_true : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct
  type definition_info = string * [ `Exists | `DoesntExist ] * [ `Negated | `NotNegated ]

  type t =
    [ `Null
    | `String of string
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Array of t array
    | `Record of t StringMap.t
    | `Function of function_info
    | `DefinitionInfo of definition_info
    | `TagInfo of Tag.info
    | `TemplateNode of
      [ `Component of unit -> t | `Html ] * string * t StringMap.t * t list * bool
    ]

  and function_info =
    { parameters : string list
    ; state : State.t
    ; exec : arguments:t StringMap.t -> state:State.t -> unit -> t
    }

  let null () = `Null
  let of_string s = `String s
  let of_bool b = `Bool b
  let of_int i = `Int i
  let of_float f = `Float f
  let of_list l = `Array (Array.of_list l)
  let of_string_map m = `Record m

  let make_component ~render ~tag ~attributes ~children =
    `TemplateNode (`Component render, tag, attributes, children, false)
  ;;

  let rec to_string = function
    | `Null -> ""
    | `String s -> s
    | `Int i -> string_of_int i
    | `Float f when Float.is_integer f -> string_of_int (int_of_float f)
    | `Float f -> string_of_float f
    | `Bool b -> if b then "true" else "false"
    | `Array l ->
      let buf = Buffer.create 200 in
      l
      |> Array.iteri (fun i it ->
             if i <> 0 then Buffer.add_char buf '\n';
             Buffer.add_string buf (to_string it));
      Buffer.contents buf
    | `Record m ->
      let b = Buffer.create 1024 in
      m
      |> StringMap.iter (fun _key value ->
             Buffer.add_string b (to_string value);
             Buffer.add_char b '\n');
      Buffer.contents b
    | `TemplateNode (`Html, tag, attributes, children, self_closing) ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf '<';
      Buffer.add_string buf tag;
      if not (StringMap.is_empty attributes)
      then
        attributes
        |> StringMap.iter (fun key value ->
               match value with
               | `Null -> ()
               | `Function _
               | `String _
               | `Int _
               | `Float _
               | `Bool _
               | `Array _
               | `Record _
               | `TemplateNode _
               | `DefinitionInfo _ ->
                 Buffer.add_char buf ' ';
                 Buffer.add_string buf key;
                 Buffer.add_char buf '=';
                 Buffer.add_char buf '"';
                 Buffer.add_string buf (value |> to_string);
                 Buffer.add_char buf '"'
               | `TagInfo _ -> assert false);
      if self_closing && Pinc_HTML.is_void_el tag
      then Buffer.add_string buf " />"
      else (
        Buffer.add_char buf '>';
        children |> List.iter (fun child -> Buffer.add_string buf (to_string child));
        Buffer.add_char buf '<';
        Buffer.add_char buf '/';
        Buffer.add_string buf tag;
        Buffer.add_char buf '>');
      Buffer.contents buf
    | `TemplateNode
        (`Component render_fn, _tag, _attributes, _slotted_children, _self_closing) ->
      render_fn () |> to_string
    | `Function _ -> ""
    | `DefinitionInfo _ -> ""
    | `TagInfo _ -> assert false
  ;;

  let is_true = function
    | `Null -> false
    | `Bool b -> b
    | `String s -> s |> String.trim |> String.length > 0
    | `Int _ -> true
    | `Float _ -> true
    | `TemplateNode _ -> true
    | `DefinitionInfo (_name, `Exists, _negated) -> true
    | `DefinitionInfo (_name, `DoesntExist, _negated) -> false
    | `Function _ -> true
    | `Array [||] -> false
    | `Array _ -> true
    | `Record m -> not (StringMap.is_empty m)
    | `TagInfo _ -> assert false
  ;;

  let rec equal a b =
    match a, b with
    | `String a, `String b -> String.equal a b
    | `Int a, `Int b -> a = b
    | `Float a, `Float b -> a = b
    | `Float a, `Int b -> a = float_of_int b
    | `Int a, `Float b -> float_of_int a = b
    | `Bool a, `Bool b -> a = b
    | `Array a, `Array b -> a = b
    | `Record a, `Record b -> StringMap.equal equal a b
    | `Function _, `Function _ -> false
    | `DefinitionInfo (a, _, _), `DefinitionInfo (b, _, _) -> String.equal a b
    | ( `TemplateNode (a_typ, a_tag, a_attrs, a_children, a_self_closing)
      , `TemplateNode (b_typ, b_tag, b_attrs, b_children, b_self_closing) ) ->
      a_typ = b_typ
      && a_tag = b_tag
      && a_self_closing = b_self_closing
      && StringMap.equal equal a_attrs b_attrs
      && a_children = b_children
    | `Null, `Null -> true
    | `TagInfo _, _ -> assert false
    | _, `TagInfo _ -> assert false
    | _ -> false
  ;;

  let rec compare a b =
    match a, b with
    | `String a, `String b -> String.compare a b
    | `Int a, `Int b -> Int.compare a b
    | `Float a, `Float b -> Float.compare a b
    | `Float a, `Int b -> Float.compare a (float_of_int b)
    | `Int a, `Float b -> Float.compare (float_of_int a) b
    | `Bool a, `Bool b -> Bool.compare a b
    | `Array a, `Array b -> Int.compare (Array.length a) (Array.length b)
    | `Record a, `Record b -> StringMap.compare compare a b
    | `Null, `Null -> 0
    | `TemplateNode _, `TemplateNode _ -> 0
    | `DefinitionInfo _, `DefinitionInfo _ -> 0
    | `Function _, `Function _ -> 0
    | `TagInfo _, _ -> assert false
    | _, `TagInfo _ -> assert false
    | _ -> assert false
  ;;
end

and Tag : sig
  type info = string * bool * Value.t StringMap.t * (Value.t -> Value.t)

  type t =
    { validate : info -> Value.t -> (Value.t, string) Result.t
    ; transform : info -> Value.t -> Value.t
    ; eval : info -> (Value.t, string) Result.t
    }
end =
  Tag

and State : sig
  type t =
    { binding_identifier : (bool * string) option
    ; declarations : Pinc_Ast.declaration StringMap.t
    ; output : Value.t
    ; environment : environment
    ; tag_listeners : Tag.t StringMap.t
    ; tag_info : bool
    }

  and environment = { mutable scope : (string * binding) list list }

  and binding =
    { is_mutable : bool
    ; is_optional : bool
    ; value : Value.t
    }

  val make : ?tag_listeners:Tag.t StringMap.t -> Pinc_Ast.declaration StringMap.t -> t
  val add_scope : t -> t

  val add_value_to_scope
    :  ident:string
    -> value:Value.t
    -> is_optional:bool
    -> is_mutable:bool
    -> t
    -> t

  val add_value_to_function_scopes
    :  ident:string
    -> value:Value.t
    -> is_optional:bool
    -> is_mutable:bool
    -> t
    -> unit

  val update_value_in_scope : ident:string -> value:Value.t -> t -> unit
  val get_value_from_scope : ident:string -> t -> binding option
  val get_output : t -> Value.t
  val add_output : output:Value.t -> t -> t
  val get_bindings : t -> (string * binding) list
  val call_tag_listener : key:string -> tag:Tag.info -> t -> Value.t
end = struct
  type t =
    { binding_identifier : (bool * string) option
    ; declarations : Pinc_Ast.declaration StringMap.t
    ; output : Value.t
    ; environment : environment
    ; tag_listeners : Tag.t StringMap.t
    ; tag_info : bool
    }

  and environment = { mutable scope : (string * binding) list list }

  and binding =
    { is_mutable : bool
    ; is_optional : bool
    ; value : Value.t
    }

  let make ?(tag_listeners = StringMap.empty) declarations =
    { binding_identifier = None
    ; declarations
    ; output = `Null
    ; environment = { scope = [] }
    ; tag_listeners
    ; tag_info = false
    }
  ;;

  let add_scope t =
    let environment = { scope = [] :: t.environment.scope } in
    { t with environment }
  ;;

  let add_value_to_scope ~ident ~value ~is_optional ~is_mutable t =
    let update_scope t =
      match t.environment.scope with
      | [] -> assert false
      | scope :: rest -> ((ident, { is_mutable; is_optional; value }) :: scope) :: rest
    in
    let environment = { scope = update_scope t } in
    { t with environment }
  ;;

  let update_value_in_scope ~ident ~value t =
    let updated = ref false in
    let rec update_scope state =
      List.map
        (function
          | scope when not !updated ->
            (List.map (function
                | key, binding when (not !updated) && key = ident && binding.is_mutable ->
                  updated := true;
                  key, { binding with value }
                | ( key
                  , ({ value = `Function { state = fn_state; parameters; exec }; _ } as
                    binding) )
                  when not !updated ->
                  fn_state.environment.scope <- update_scope fn_state;
                  ( key
                  , { binding with
                      value = `Function { state = fn_state; parameters; exec }
                    } )
                | v -> v))
              scope
          | scope -> scope)
        state.environment.scope
    in
    t.environment.scope <- update_scope t
  ;;

  let add_value_to_function_scopes ~ident ~value ~is_optional ~is_mutable t =
    let update_scope state =
      List.map
        (List.map (function
            | key, ({ value = `Function { state; parameters; exec }; _ } as binding) ->
              let new_state =
                add_value_to_scope ~ident ~value ~is_optional ~is_mutable state
              in
              ( key
              , { binding with value = `Function { state = new_state; parameters; exec } }
              )
            | v -> v))
        state.environment.scope
    in
    t.environment.scope <- update_scope t
  ;;

  let get_value_from_scope ~ident t =
    t.environment.scope |> List.find_map (List.assoc_opt ident)
  ;;

  let get_output t = t.output
  let add_output ~output t = { t with output }
  let get_bindings t = t.environment.scope |> List.hd

  let call_tag_listener ~key ~tag t =
    match t.tag_listeners |> StringMap.find_opt key with
    | None -> `Null
    | Some listener ->
      tag
      |> listener.eval
      |> (function
      | Ok v -> v
      | Error e -> failwith e)
  ;;
end
