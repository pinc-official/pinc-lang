module Parser = Pinc_Parser
module Ast = Pinc_Ast
module StringMap = Ast.StringMap

exception Loop_Break
exception Loop_Continue

let should_never_happen () =
  let exception Internal_Error in
  raise Internal_Error
;;

module rec Value : sig
  type definition_info =
    { name : string
    ; exists : bool
    ; negated : bool
    }

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
    | `TagInfo of Tag.t
    | `TemplateNode of
      [ `Component of models:(string -> t option) -> slotted_children:t list -> t
      | `Html
      ]
      * string
      * t StringMap.t
      * t list
      * bool
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
    :  render:(models:(string -> t option) -> slotted_children:t list -> t)
    -> tag:string
    -> attributes:t StringMap.t
    -> children:t list
    -> t

  val to_string : t -> string
  val is_true : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct
  type definition_info =
    { name : string
    ; exists : bool
    ; negated : bool
    }

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
    | `TagInfo of Tag.t
    | `TemplateNode of
      [ `Component of models:(string -> t option) -> slotted_children:t list -> t
      | `Html
      ]
      * string
      * t StringMap.t
      * t list
      * bool
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
               | `TagInfo _ -> should_never_happen ());
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
        (`Component render_fn, _tag, attributes, slotted_children, _self_closing) ->
      let models s = attributes |> StringMap.find_opt s in
      render_fn ~models ~slotted_children |> to_string
    | `Function _ -> ""
    | `DefinitionInfo _ -> ""
    | `TagInfo _ -> should_never_happen ()
  ;;

  let is_true = function
    | `Null -> false
    | `Bool b -> b
    | `String s -> s |> String.trim |> String.length > 0
    | `Int _ -> true
    | `Float _ -> true
    | `TemplateNode _ -> true
    | `DefinitionInfo { exists; _ } -> exists
    | `Function _ -> true
    | `Array [||] -> false
    | `Array _ -> true
    | `Record m -> not (StringMap.is_empty m)
    | `TagInfo _ -> should_never_happen ()
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
    | `DefinitionInfo { name = a; _ }, `DefinitionInfo { name = b; _ } -> String.equal a b
    | ( `TemplateNode (a_typ, a_tag, a_attrs, a_children, a_self_closing)
      , `TemplateNode (b_typ, b_tag, b_attrs, b_children, b_self_closing) ) ->
      a_typ = b_typ
      && a_tag = b_tag
      && a_self_closing = b_self_closing
      && StringMap.equal equal a_attrs b_attrs
      && a_children = b_children
    | `Null, `Null -> true
    | `TagInfo _, _ -> should_never_happen ()
    | _, `TagInfo _ -> should_never_happen ()
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
    | `TagInfo _, _ -> should_never_happen ()
    | _, `TagInfo _ -> should_never_happen ()
    | _ -> should_never_happen ()
  ;;
end

and Tag : sig
  type t =
    { name : string
    ; is_optional : bool
    ; attributes : Value.t StringMap.t
    ; transformer : Value.t -> Value.t
    }
end =
  Tag

and State : sig
  type t =
    { binding_identifier : (bool * string) option
    ; models : string -> Value.t option
    ; slotted_children : Value.t list option
    ; declarations : Ast.declaration StringMap.t
    ; output : Value.t
    ; environment : environment
    ; tag_listeners : (Tag.t -> Value.t) StringMap.t
    ; tag_info : bool
    }

  and environment = { mutable scope : (string * binding) list list }

  and binding =
    { is_mutable : bool
    ; is_optional : bool
    ; value : Value.t
    }

  val make
    :  ?models:(string -> Value.t option)
    -> ?tag_listeners:(Tag.t -> Value.t) StringMap.t
    -> ?slotted_children:Value.t list
    -> Ast.declaration StringMap.t
    -> t

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
  val call_tag_listener : key:string -> tag:Tag.t -> t -> Value.t
end = struct
  type t =
    { binding_identifier : (bool * string) option
    ; models : string -> Value.t option
    ; slotted_children : Value.t list option
    ; declarations : Ast.declaration StringMap.t
    ; output : Value.t
    ; environment : environment
    ; tag_listeners : (Tag.t -> Value.t) StringMap.t
    ; tag_info : bool
    }

  and environment = { mutable scope : (string * binding) list list }

  and binding =
    { is_mutable : bool
    ; is_optional : bool
    ; value : Value.t
    }

  let make
      ?(models = fun _ -> None)
      ?(tag_listeners = StringMap.empty)
      ?slotted_children
      declarations
    =
    { binding_identifier = None
    ; models
    ; slotted_children
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
      | [] -> should_never_happen ()
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
    | Some listener -> tag |> listener
  ;;
end

module StringTag = struct
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Int _ -> failwith "tried to assign integer value to a string tag."
    | `Float _ -> failwith "tried to assign float value to a string tag."
    | `Bool _ -> failwith "tried to assign boolean value to a string tag."
    | `Array _ -> failwith "tried to assign array value to a string tag."
    | `Record _ -> failwith "tried to assign record value to a string tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a string tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a string tag."
    | `Function _ -> failwith "tried to assign function to a string tag."
    | `Null -> `Null
    | `String s -> `String s
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let key =
      Ast.StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #String"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #String to be of type string"
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    data
    |> Ast.StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> tag_info.transformer
  ;;
end

module IntTag = struct
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Float _ -> failwith "tried to assign float value to a int tag."
    | `Bool _ -> failwith "tried to assign boolean value to a int tag."
    | `Array _ -> failwith "tried to assign array value to a int tag."
    | `String _ -> failwith "tried to assign string value to a int tag."
    | `Record _ -> failwith "tried to assign record value to a int tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a int tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a int tag."
    | `Function _ -> failwith "tried to assign function to a int tag."
    | `Null -> `Null
    | `Int i -> `Int i
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let key =
      Ast.StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Int"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Int to be of type string"
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    data
    |> Ast.StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> tag_info.transformer
  ;;
end

module FloatTag = struct
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Bool _ -> failwith "tried to assign boolean value to a float tag."
    | `Array _ -> failwith "tried to assign array value to a float tag."
    | `String _ -> failwith "tried to assign string value to a float tag."
    | `Int _ -> failwith "tried to assign int value to a float tag."
    | `Record _ -> failwith "tried to assign record value to a float tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a float tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a float tag."
    | `Function _ -> failwith "tried to assign function to a float tag."
    | `Null -> `Null
    | `Float f -> `Float f
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let key =
      Ast.StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Float"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Float to be of type string"
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    data
    |> Ast.StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> tag_info.transformer
  ;;
end

module BooleanTag = struct
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Array _ -> failwith "tried to assign array value to a boolean tag."
    | `String _ -> failwith "tried to assign string value to a boolean tag."
    | `Int _ -> failwith "tried to assign int value to a boolean tag."
    | `Float _ -> failwith "tried to assign float value to a boolean tag."
    | `Record _ -> failwith "tried to assign record value to a boolean tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a boolean tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a boolean tag."
    | `Function _ -> failwith "tried to assign function to a boolean tag."
    | `Null -> `Null
    | `Bool b -> `Bool b
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let key =
      Ast.StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Boolean"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Boolean to be of type string"
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    data
    |> Ast.StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> tag_info.transformer
  ;;
end

module ArrayTag = struct
  (* TODO: Validate the underlying values with their tags validator *)
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Bool _ -> failwith "tried to assign boolean value to a array tag."
    | `String _ -> failwith "tried to assign string value to a array tag."
    | `Int _ -> failwith "tried to assign int value to a array tag."
    | `Float _ -> failwith "tried to assign float value to a array tag."
    | `Record _ -> failwith "tried to assign record value to a array tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a array tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a array tag."
    | `Function _ -> failwith "tried to assign function to a array tag."
    | `Null -> `Null
    | `Array a -> `Array a
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let key =
      Ast.StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Array"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Array to be of type string"
    in
    let of' =
      Ast.StringMap.find_opt "of" attributes
      |> function
      | None -> failwith "Expected attribute `of` to exist on #Array"
      | Some (`TagInfo i) -> i
      | Some _ ->
        failwith
          "Expected attribute `of` #Array to be a tag describing the type of the items \
           inside."
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    data
    |> Ast.StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> (function
         | `Array a -> `Array (a |> Array.map (fun value -> of'.transformer value))
         | `Null -> `Null)
    |> tag_info.transformer
  ;;
end

module RecordTag = struct
  (* TODO: Validate the underlying values with their tags validator *)
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Bool _ -> failwith "tried to assign boolean value to a record tag."
    | `String _ -> failwith "tried to assign string value to a record tag."
    | `Int _ -> failwith "tried to assign int value to a record tag."
    | `Float _ -> failwith "tried to assign float value to a record tag."
    | `Array _ -> failwith "tried to assign array value to a record tag."
    | `TemplateNode _ -> failwith "tried to assign template node to a record tag."
    | `DefinitionInfo _ -> failwith "tried to assign definition info to a record tag."
    | `Function _ -> failwith "tried to assign function to a record tag."
    | `Null -> `Null
    | `Record r -> `Record r
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let key =
      Ast.StringMap.find_opt "key" attributes
      |> function
      | None -> failwith "Expected attribute `key` to exist on #Record"
      | Some (`String s) -> s
      | Some _ -> failwith "Expected attribute `key` #Record to be of type string"
    in
    let of' =
      Ast.StringMap.find_opt "of" attributes
      |> function
      | None -> failwith "Expected attribute `of` to exist on #Record"
      | Some (`Record r) ->
        r
        |> StringMap.filter_map (fun _key -> function
             | `TagInfo i -> Some i
             | _ -> None)
      | Some _ ->
        failwith
          "Expected attribute `of` #Array to be a tag describing the type of the items \
           inside."
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    data
    |> Ast.StringMap.find_opt key
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> (function
         | `Record r ->
           `Record
             (r
             |> StringMap.mapi (fun key value ->
                    of'
                    |> StringMap.find_opt key
                    |> function
                    | Some info -> info.transformer value
                    | None -> value))
         | `Null -> `Null)
    |> tag_info.transformer
  ;;
end

module SlotTag = struct
  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let slot_name =
      attributes
      |> StringMap.find_opt "name"
      |> Option.value ~default:(`String "")
      |> function
      | `String s -> s
      | _ -> failwith "Expected attribute `name` on #Slot to be of type string."
    in
    let min =
      attributes
      |> StringMap.find_opt "min"
      |> Option.value ~default:(`Int 0)
      |> function
      | `Int i -> i
      | _ -> failwith "Expected attribute `min` on #Slot to be of type int."
    in
    let max =
      attributes
      |> StringMap.find_opt "max"
      |> function
      | None -> None
      | Some (`Int i) -> Some i
      | _ -> failwith "Expected attribute `max` on #Slot to be of type int."
    in
    let instanceOf =
      attributes
      |> StringMap.find_opt "instanceOf"
      |> function
      | None -> None
      | Some (`Array l) ->
        Some
          (l
          |> Array.map (function
                 | `DefinitionInfo info -> info
                 | _ ->
                   failwith
                     "Expected attribute `instanceOf` on #Slot to be an array of \
                      uppercase identifiers."))
      | _ -> failwith "Expected attribute `instanceOf` on #Slot to be an array."
    in
    let find_slot_key attributes =
      attributes
      |> StringMap.find_opt "slot"
      |> Option.value ~default:(`String "")
      |> function
      | `String s -> s
      | _ -> failwith "Expected slot attribute to be of type string"
    in
    let check_instance_restriction tag f =
      match instanceOf with
      | None -> f
      | Some restrictions ->
        let is_in_list = ref false in
        let allowed, disallowed =
          restrictions
          |> Array.to_list
          |> List.partition_map (fun Value.{ name; negated; _ } ->
                 if name = tag then is_in_list := true;
                 if negated then Either.right name else Either.left name)
        in
        let is_in_list = !is_in_list in
        let is_allowed =
          match allowed, disallowed with
          | [], _disallowed -> not is_in_list
          | _allowed, [] -> is_in_list
          | allowed, _disallowed -> List.mem tag allowed
        in
        if not is_allowed
        then
          failwith
            ("Child with tag `"
            ^ tag
            ^ "` may not be used inside the "
            ^ (if slot_name = ""
              then "Default #Slot."
              else "#Slot with name `" ^ slot_name ^ "`")
            ^ ". The following restrictions are set: [ "
            ^ (instanceOf
              |> Option.value ~default:[||]
              |> Array.to_list
              |> List.map (fun res ->
                     (if res.Value.negated then "!" else "") ^ res.Value.name)
              |> String.concat ",")
            ^ " ]")
        else f
    in
    let rec keep_slotted acc = function
      | ( `TemplateNode (`Html, tag, attributes, _children, _self_closing)
        | `TemplateNode (`Component _, tag, attributes, _children, _self_closing) ) as
        value ->
        if find_slot_key attributes = slot_name
        then check_instance_restriction tag @@ Array.append acc [| value |]
        else acc
      | `Array l -> l |> Array.fold_left keep_slotted acc
      | `String s when String.trim s = "" -> acc
      | _ ->
        failwith
          "Only nodes may be placed into slots. If you want to put a plain text into a \
           slot, you have to wrap it in a <p></p> tag for example."
    in
    let slotted_children = data |> Array.of_list |> Array.fold_left keep_slotted [||] in
    let amount_of_children = Array.length slotted_children in
    match slot_name, min, amount_of_children, max with
    | "", min, len, _ when len < min ->
      failwith
        ("Default #Slot did not reach the minimum amount of nodes (specified as "
        ^ string_of_int min
        ^ ").")
    | slot_name, min, len, _ when len < min ->
      failwith
        ("#Slot with name `"
        ^ slot_name
        ^ "` did not reach the minimum amount of nodes (specified as "
        ^ string_of_int min
        ^ ").")
    | "", _, len, Some max when len > max ->
      failwith
        ("Default #Slot includes more than the maximum amount of nodes (specified as "
        ^ string_of_int max
        ^ ").")
    | slot_name, _, len, Some max when len > max ->
      failwith
        ("#Slot with name `"
        ^ slot_name
        ^ "` includes more than the maximum amount of nodes (specified as "
        ^ string_of_int max
        ^ ").")
    | _ -> `Array slotted_children |> tag_info.transformer
  ;;
end

module SetContextTag = struct
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | `Bool _ -> should_never_happen ()
    | `String _ -> should_never_happen ()
    | `Int _ -> should_never_happen ()
    | `Float _ -> should_never_happen ()
    | `Array _ -> should_never_happen ()
    | `TemplateNode _ -> should_never_happen ()
    | `DefinitionInfo _ -> should_never_happen ()
    | `Function _ -> should_never_happen ()
    | `Record _ -> should_never_happen ()
    | `Null -> `Null
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let name =
      attributes
      |> StringMap.find_opt "name"
      |> function
      | Some (`String s) -> s
      | None -> failwith "attribute name is required when setting a context."
      | Some _ ->
        failwith "Expected attribute `name` on #SetContext to be of type string."
    in
    let value =
      attributes
      |> StringMap.find_opt "value"
      |> function
      | Some value -> value
      | None -> failwith "attribute value is required when setting a context."
    in
    Hashtbl.add data name value;
    Value.null () |> validate |> tag_info.transformer
  ;;
end

module GetContextTag = struct
  let validate = function
    | `TagInfo _ -> should_never_happen ()
    | ( `Bool _
      | `String _
      | `Int _
      | `Float _
      | `Array _
      | `TemplateNode _
      | `DefinitionInfo _
      | `Function _
      | `Record _
      | `Null ) as v -> v
  ;;

  let get_value data (tag_info : Tag.t) =
    let attributes = tag_info.attributes in
    let name =
      attributes
      |> StringMap.find_opt "name"
      |> function
      | Some (`String s) -> s
      | None -> failwith "attribute name is required when getting a context."
      | Some _ ->
        failwith "Expected attribute `name` on #GetContext to be of type string."
    in
    let default = Ast.StringMap.find_opt "default" attributes in
    Hashtbl.find_opt data name
    |> (function
         | None -> default
         | Some v -> Some v)
    |> Option.value ~default:(Value.null ())
    |> validate
    |> tag_info.transformer
  ;;
end

let rec eval_statement ~state = function
  | Ast.LetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:false ~is_optional:false expression
  | Ast.OptionalLetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:false ~is_optional:true expression
  | Ast.OptionalMutableLetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:true ~is_optional:true expression
  | Ast.MutableLetStatement (Lowercase_Id ident, expression) ->
    eval_let ~state ~ident ~is_mutable:true ~is_optional:false expression
  | Ast.MutationStatement (Lowercase_Id ident, expression) ->
    eval_mutation ~state ~ident expression
  | Ast.BreakStatement -> raise_notrace Loop_Break
  | Ast.ContinueStatement -> raise_notrace Loop_Continue
  | Ast.ExpressionStatement expression -> expression |> eval_expression ~state

and eval_expression ~state = function
  | Ast.Int i -> state |> State.add_output ~output:(`Int i)
  | Ast.Float f when Float.is_integer f ->
    state |> State.add_output ~output:(`Int (int_of_float f))
  | Ast.Float f -> state |> State.add_output ~output:(`Float f)
  | Ast.Bool b -> state |> State.add_output ~output:(`Bool b)
  | Ast.Array l ->
    state
    |> State.add_output
         ~output:
           (`Array
             (l |> Array.map (fun it -> it |> eval_expression ~state |> State.get_output)))
  | Ast.Record map ->
    state
    |> State.add_output
         ~output:
           (`Record
             (map
             |> Ast.StringMap.mapi (fun ident (optional, expression) ->
                    expression
                    |> eval_expression
                         ~state:
                           { state with
                             State.binding_identifier = Some (optional, ident)
                           }
                    |> State.get_output
                    |> function
                    | `Null when not optional ->
                      failwith
                        ("identifier "
                        ^ ident
                        ^ " is not marked as nullable, but was given a null value.")
                    | value -> value)))
  | Ast.String template -> eval_string_template ~state template
  | Ast.Function (parameters, body) -> eval_function_declaration ~state ~parameters body
  | Ast.FunctionCall (left, arguments) -> eval_function_call ~state ~arguments left
  | Ast.UppercaseIdentifierExpression (Uppercase_Id id) ->
    let value = state.State.declarations |> StringMap.find_opt id in
    state
    |> State.add_output
         ~output:
           (`DefinitionInfo { name = id; exists = Option.is_some value; negated = false })
  | Ast.LowercaseIdentifierExpression (Lowercase_Id id) ->
    eval_lowercase_identifier ~state id
  | Ast.TagExpression tag -> eval_tag ~state tag
  | Ast.ForInExpression { index; iterator = Lowercase_Id ident; reverse; iterable; body }
    -> eval_for_in ~state ~index_ident:index ~ident ~reverse ~iterable body
  | Ast.TemplateExpression nodes ->
    state
    |> State.add_output
         ~output:
           (`Array
             (nodes
             |> List.map (fun it -> it |> eval_template ~state |> State.get_output)
             |> Array.of_list))
  | Ast.BlockExpression e -> eval_block ~state e
  | Ast.ConditionalExpression { condition; consequent; alternate } ->
    eval_if ~state ~condition ~alternate ~consequent
  | Ast.UnaryExpression (Ast.Operators.Unary.NOT, expression) ->
    eval_unary_not ~state expression
  | Ast.UnaryExpression (Ast.Operators.Unary.MINUS, expression) ->
    eval_unary_minus ~state expression
  | Ast.BinaryExpression (left, Ast.Operators.Binary.EQUAL, right) ->
    eval_binary_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.NOT_EQUAL, right) ->
    eval_binary_not_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.GREATER, right) ->
    eval_binary_greater ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.GREATER_EQUAL, right) ->
    eval_binary_greater_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.LESS, right) ->
    eval_binary_less ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.LESS_EQUAL, right) ->
    eval_binary_less_equal ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.PLUS, right) ->
    eval_binary_plus ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MINUS, right) ->
    eval_binary_minus ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.TIMES, right) ->
    eval_binary_times ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.DIV, right) ->
    eval_binary_div ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.POW, right) ->
    eval_binary_pow ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MODULO, right) ->
    eval_binary_modulo ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.CONCAT, right) ->
    eval_binary_concat ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.AND, right) ->
    eval_binary_and ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.OR, right) ->
    eval_binary_or ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.DOT_ACCESS, right) ->
    eval_binary_dot_access ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.BRACKET_ACCESS, right) ->
    eval_binary_bracket_access ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.ARRAY_ADD, right) ->
    eval_binary_array_add ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.MERGE, right) ->
    eval_binary_merge ~state left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.RANGE, right) ->
    eval_range ~state ~inclusive:false left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.INCLUSIVE_RANGE, right) ->
    eval_range ~state ~inclusive:true left right
  | Ast.BinaryExpression (left, Ast.Operators.Binary.FUNCTION_CALL, right) ->
    eval_function_call ~state ~arguments:[ right ] left
  | Ast.BinaryExpression (left, Ast.Operators.Binary.PIPE, right) ->
    eval_binary_pipe ~state left right

and eval_string_template ~state template =
  state
  |> State.add_output
       ~output:
         (`String
           (template
           |> List.map (function
                  | Ast.StringText s -> s
                  | Ast.StringInterpolation e ->
                    eval_expression ~state e |> State.get_output |> Value.to_string)
           |> String.concat ""))

and eval_function_declaration ~state ~parameters body =
  let ident = state.State.binding_identifier in
  let self = ref `Null in
  let exec ~arguments ~state () =
    let state =
      match ident with
      | None -> state
      | Some (_, ident) ->
        state
        |> State.add_value_to_scope
             ~ident
             ~value:!self
             ~is_mutable:false
             ~is_optional:false
    in
    let state =
      state
      |> State.add_scope
      |> StringMap.fold
           (fun ident value ->
             State.add_value_to_scope ~ident ~value ~is_mutable:false ~is_optional:false)
           arguments
    in
    eval_expression ~state body |> State.get_output
  in
  let fn = `Function Value.{ parameters; state; exec } in
  ident
  |> Option.iter (fun (_, ident) ->
         state
         |> State.add_value_to_function_scopes
              ~ident
              ~value:fn
              ~is_optional:false
              ~is_mutable:false);
  self := fn;
  state |> State.add_output ~output:fn

and eval_function_call ~state ~arguments left =
  let maybe_fn = eval_expression ~state left |> State.get_output in
  match maybe_fn with
  | `Function { parameters; state = fn_state; exec }
    when List.compare_lengths parameters arguments = 0 ->
    let arguments =
      List.combine parameters arguments
      |> List.fold_left
           (fun acc (param, arg) ->
             let value = arg |> eval_expression ~state |> State.get_output in
             acc |> StringMap.add param value)
           StringMap.empty
    in
    state |> State.add_output ~output:(exec ~arguments ~state:fn_state ())
  | `Function { parameters; state = _; exec = _ } ->
    if List.compare_lengths parameters arguments > 0
    then (
      let arguments_len = List.length arguments in
      let missing =
        parameters
        |> List.filteri (fun index _ -> index > arguments_len - 1)
        |> List.map (fun item -> "`" ^ item ^ "`")
        |> String.concat ", "
      in
      failwith
        ("This function was provided too few arguments. The following parameters are \
          missing: "
        ^ missing))
    else
      failwith
        ("This function only accepts "
        ^ string_of_int (List.length parameters)
        ^ " arguments, but was provided "
        ^ string_of_int (List.length arguments)
        ^ " here.")
  | _ -> failwith "Trying to call a non function value"

and eval_binary_pipe ~state left right =
  let right =
    match right with
    | Ast.FunctionCall (fn, arguments) -> Ast.FunctionCall (fn, left :: arguments)
    | fn -> Ast.FunctionCall (fn, [ left ])
  in
  right |> eval_expression ~state

and eval_binary_plus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `Int a, `Int b -> state |> State.add_output ~output:(`Int (a + b))
  | `Float a, `Float b -> state |> State.add_output ~output:(`Float (a +. b))
  | `Float a, `Int b -> state |> State.add_output ~output:(`Float (a +. float_of_int b))
  | `Int a, `Float b -> state |> State.add_output ~output:(`Float (float_of_int a +. b))
  | _ -> failwith "Trying to add non numeric literals."

and eval_binary_minus ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `Int a, `Int b -> state |> State.add_output ~output:(`Int (a - b))
  | `Float a, `Float b -> state |> State.add_output ~output:(`Float (a -. b))
  | `Float a, `Int b -> state |> State.add_output ~output:(`Float (a -. float_of_int b))
  | `Int a, `Float b -> state |> State.add_output ~output:(`Float (float_of_int a -. b))
  | _ -> failwith "Trying to subtract non numeric literals."

and eval_binary_times ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `Int a, `Int b -> state |> State.add_output ~output:(`Int (a * b))
  | `Float a, `Float b -> state |> State.add_output ~output:(`Float (a *. b))
  | `Float a, `Int b -> state |> State.add_output ~output:(`Float (a *. float_of_int b))
  | `Int a, `Float b -> state |> State.add_output ~output:(`Float (float_of_int a *. b))
  | _ -> failwith "Trying to multiply non numeric literals."

and eval_binary_div ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let r =
    match a, b with
    | `Int _, `Int 0 -> failwith "Trying to divide by 0"
    | `Float _, `Float 0. -> failwith "Trying to divide by 0"
    | `Float _, `Int 0 -> failwith "Trying to divide by 0"
    | `Int _, `Float 0. -> failwith "Trying to divide by 0"
    | `Int a, `Int b -> float_of_int a /. float_of_int b
    | `Float a, `Float b -> a /. b
    | `Float a, `Int b -> a /. float_of_int b
    | `Int a, `Float b -> float_of_int a /. b
    | _ -> failwith "Trying to divide non numeric literals."
  in
  if Float.is_integer r
  then state |> State.add_output ~output:(`Int (int_of_float r))
  else state |> State.add_output ~output:(`Float r)

and eval_binary_pow ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let r =
    match a, b with
    | `Int a, `Int b -> float_of_int a ** float_of_int b
    | `Float a, `Float b -> a ** b
    | `Float a, `Int b -> a ** float_of_int b
    | `Int a, `Float b -> float_of_int a ** b
    | _ -> failwith "Trying to raise non numeric literals."
  in
  if Float.is_integer r
  then state |> State.add_output ~output:(`Int (int_of_float r))
  else state |> State.add_output ~output:(`Float r)

and eval_binary_modulo ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  let ( %. ) = mod_float in
  let ( % ) = ( mod ) in
  let r =
    match a, b with
    | `Int _, `Int 0 -> failwith "Trying to modulo with 0 on right hand side."
    | `Int _, `Float 0. -> failwith "Trying to modulo with 0 on right hand side."
    | `Float _, `Float 0. -> failwith "Trying to modulo with 0 on right hand side."
    | `Float _, `Int 0 -> failwith "Trying to modulo with 0 on right hand side."
    | `Int a, `Int b -> a % b
    | `Float a, `Float b -> int_of_float (a %. b)
    | `Float a, `Int b -> int_of_float a % b
    | `Int a, `Float b -> a % int_of_float b
    | _ -> failwith "Trying to modulo non numeric literals."
  in
  state |> State.add_output ~output:(`Int r)

and eval_binary_and ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.is_true a && Value.is_true b))

and eval_binary_or ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.is_true a || Value.is_true b))

and eval_binary_less ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b < 0))

and eval_binary_less_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b <= 0))

and eval_binary_greater ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b > 0))

and eval_binary_greater_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.compare a b >= 0))

and eval_binary_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (Value.equal a b))

and eval_binary_not_equal ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  state |> State.add_output ~output:(`Bool (not (Value.equal a b)))

and eval_binary_concat ~state left right =
  let a = left |> eval_expression ~state |> State.get_output in
  let b = right |> eval_expression ~state |> State.get_output in
  match a, b with
  | `String a, `String b -> state |> State.add_output ~output:(`String (a ^ b))
  | _ -> failwith "Trying to concat non string literals."

and eval_binary_dot_access ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Record left, Ast.LowercaseIdentifierExpression (Lowercase_Id b) ->
    let output = left |> StringMap.find_opt b |> Option.value ~default:`Null in
    state |> State.add_output ~output
  | ( `TemplateNode (_typ, tag, attributes, children, _self_closing)
    , Ast.LowercaseIdentifierExpression (Lowercase_Id b) ) ->
    (match b with
    | "tag" -> state |> State.add_output ~output:(`String tag)
    | "attributes" -> state |> State.add_output ~output:(`Record attributes)
    | "children" -> state |> State.add_output ~output:(`Array (Array.of_list children))
    | s ->
      failwith
        ("Unknown property "
        ^ s
        ^ " on template node. Known properties are: `tag`, `attributes` and `children`."))
  | `Record _, _ ->
    failwith "Expected right hand side of record access to be a lowercase identifier."
  | `Null, _ -> state |> State.add_output ~output:`Null
  | _, Ast.LowercaseIdentifierExpression _ ->
    failwith "Trying to access a property on a non record or template value."
  | _ -> failwith "I am really not sure what you are trying to do here..."

and eval_binary_bracket_access ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Array left, `Int right ->
    let output =
      try left.(right) with
      | Invalid_argument _ -> `Null
    in
    state |> State.add_output ~output
  | `Record left, `String right ->
    let output = left |> StringMap.find_opt right |> Option.value ~default:`Null in
    state |> State.add_output ~output
  | `Null, _ -> state |> State.add_output ~output:`Null
  | `Array _, _ -> failwith "Cannot access array with a non integer value."
  | `Record _, _ -> failwith "Cannot access record with a non string value."
  | _ -> failwith "Trying to access a property on a non record or array value."

and eval_binary_array_add ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Array left, value ->
    state |> State.add_output ~output:(`Array (Array.append left [| value |]))
  | _ -> failwith "Trying to add an element on a non array value."

and eval_binary_merge ~state left right =
  let left = left |> eval_expression ~state |> State.get_output in
  let right = right |> eval_expression ~state |> State.get_output in
  match left, right with
  | `Array left, `Array right ->
    state |> State.add_output ~output:(`Array (Array.append left right))
  | `Record left, `Record right ->
    state
    |> State.add_output
         ~output:
           (`Record
             (StringMap.merge
                (fun _ x y ->
                  match x, y with
                  | (Some _ | None), Some y -> Some y
                  | Some x, None -> Some x
                  | None, None -> None)
                left
                right))
  | `TemplateNode (typ, tag, attributes, children, self_closing), `Record right ->
    let attributes =
      StringMap.merge
        (fun _ x y ->
          match x, y with
          | (Some _ | None), Some y -> Some y
          | Some x, None -> Some x
          | None, None -> None)
        attributes
        right
    in
    state
    |> State.add_output
         ~output:(`TemplateNode (typ, tag, attributes, children, self_closing))
  | `TemplateNode _, _ ->
    failwith "Trying to merge a non record value onto tag attributes."
  | `Array _, _ -> failwith "Trying to merge a non array value onto an array."
  | _, `Array _ -> failwith "Trying to merge an array value onto a non array."
  | _ -> failwith "Trying to merge two non array values."

and eval_unary_not ~state expression =
  match eval_expression ~state expression |> State.get_output with
  | `DefinitionInfo info ->
    state
    |> State.add_output ~output:(`DefinitionInfo { info with negated = not info.negated })
  | v -> state |> State.add_output ~output:(`Bool (not (Value.is_true v)))

and eval_unary_minus ~state expression =
  match eval_expression ~state expression |> State.get_output with
  | `Int i -> state |> State.add_output ~output:(`Int (Int.neg i))
  | `Float f -> state |> State.add_output ~output:(`Float (Float.neg f))
  | _ ->
    failwith
      "Invalid usage of unary `-` operator. You are only able to negate integers or \
       floats."

and eval_lowercase_identifier ~state ident =
  state
  |> State.get_value_from_scope ~ident
  |> function
  | None -> failwith ("Unbound identifier `" ^ ident ^ "`")
  | Some { value; is_mutable = _; is_optional = _ } ->
    state |> State.add_output ~output:value

and eval_let ~state ~ident ~is_mutable ~is_optional expression =
  let state =
    expression
    |> eval_expression
         ~state:{ state with State.binding_identifier = Some (is_optional, ident) }
  in
  match State.get_output state with
  | `Null when not is_optional ->
    failwith
      ("identifier " ^ ident ^ " is not marked as nullable, but was given a null value.")
  | value ->
    state
    |> State.add_value_to_scope ~ident ~value ~is_mutable ~is_optional
    |> State.add_output ~output:`Null

and eval_mutation ~state ~ident expression =
  let current_binding = State.get_value_from_scope ~ident state in
  match current_binding with
  | None ->
    failwith "Trying to update a variable, which does not exist in the current scope."
  | Some { is_mutable = false; _ } -> failwith "Trying to update a non mutable variable."
  | Some { value = _; is_mutable = true; is_optional } ->
    let output =
      expression
      |> eval_expression
           ~state:{ state with State.binding_identifier = Some (is_optional, ident) }
    in
    let () =
      output
      |> State.get_output
      |> function
      | `Null when not is_optional ->
        failwith
          ("identifier "
          ^ ident
          ^ " is not marked as nullable, but was tried to be updated with a null value.")
      | value -> state |> State.update_value_in_scope ~ident ~value
    in
    state |> State.add_output ~output:`Null

and eval_if ~state ~condition ~alternate ~consequent =
  let condition_matches =
    condition |> eval_expression ~state |> State.get_output |> Value.is_true
  in
  match condition_matches, alternate with
  | true, _ -> consequent |> eval_statement ~state
  | false, Some alt -> alt |> eval_statement ~state
  | false, None -> state |> State.add_output ~output:`Null

and eval_for_in ~state ~index_ident ~ident ~reverse ~iterable body =
  let make_rev array =
    array |> Array.stable_sort (fun _ _ -> 1);
    array
  in
  let iterable = iterable |> eval_expression ~state |> State.get_output in
  let maybe_rev = if reverse then make_rev else fun arr -> arr in
  let index = ref (-1) in
  let rec loop acc = function
    | [] -> List.rev acc
    | value :: tl ->
      index := succ !index;
      let state =
        state
        |> State.add_value_to_scope ~ident ~value ~is_mutable:false ~is_optional:false
      in
      let state =
        match index_ident with
        | Some (Lowercase_Id ident) ->
          state
          |> State.add_value_to_scope
               ~ident
               ~value:(`Int !index)
               ~is_mutable:false
               ~is_optional:false
        | None -> state
      in
      (match eval_statement ~state body with
      | exception Loop_Continue -> loop acc tl
      | exception Loop_Break -> List.rev acc
      | v -> loop (State.get_output v :: acc) tl)
  in
  match iterable with
  | `Array l ->
    let res = l |> maybe_rev |> Array.to_list |> loop [] |> Array.of_list in
    state |> State.add_output ~output:(`Array res)
  | `String s ->
    let res =
      s
      |> String.to_seq
      |> Array.of_seq
      |> maybe_rev
      |> Array.map (fun c -> `String (String.make 1 c))
      |> Array.to_list
      |> loop []
      |> Array.of_list
    in
    state |> State.add_output ~output:(`Array res)
  | `Null -> state |> State.add_output ~output:`Null
  | `TemplateNode _ -> failwith "Cannot iterate over template node"
  | `Record _ -> failwith "Cannot iterate over record value"
  | `Int _ -> failwith "Cannot iterate over int value"
  | `Float _ -> failwith "Cannot iterate over float value"
  | `Bool _ -> failwith "Cannot iterate over boolean value"
  | `DefinitionInfo _ -> failwith "Cannot iterate over definition info"
  | `Function _ -> failwith "Cannot iterate over function"
  | `TagInfo _ -> should_never_happen ()

and eval_range ~state ~inclusive from upto =
  let from = from |> eval_expression ~state |> State.get_output in
  let upto = upto |> eval_expression ~state |> State.get_output in
  let from, upto =
    match from, upto with
    | `Int from, `Int upto -> from, upto
    | `Int _, _ ->
      failwith
        "Can't construct range in for loop. The end of your range is not of type int."
    | _, `Int _ ->
      failwith
        "Can't construct range in for loop. The start of your range is not of type int."
    | _, _ ->
      failwith
        "Can't construct range in for loop. The start and end of your range are not of \
         type int."
  in
  let iter =
    if from > upto
    then [||]
    else (
      let start = from in
      let stop = if not inclusive then upto else upto + 1 in
      Array.init (stop - start) (fun i -> `Int (i + start)))
  in
  state |> State.add_output ~output:(`Array iter)

and eval_block ~state statements =
  let state = state |> State.add_scope in
  statements |> List.fold_left (fun state -> eval_statement ~state) state

and eval_tag ~state tag =
  let Ast.{ tag_name; attributes; transformer } = tag in
  let is_optional =
    state.binding_identifier |> Option.map fst |> Option.value ~default:false
  in
  let attributes =
    attributes
    |> StringMap.map (fun it ->
           it |> eval_expression ~state:{ state with tag_info = true } |> State.get_output)
  in
  let apply_transformer ~transformer value =
    match transformer with
    | Some (Ast.Lowercase_Id ident, expr) ->
      let state =
        state
        |> State.add_scope
        |> State.add_value_to_scope ~ident ~value ~is_optional:false ~is_mutable:false
      in
      eval_expression ~state expr |> State.get_output
    | _ -> value
  in
  let attributes =
    match StringMap.find_opt "key" attributes, state.binding_identifier with
    | None, Some ident -> attributes |> StringMap.add "key" (`String (snd ident))
    | Some (`String _), _ -> attributes
    | Some _, _ -> failwith "Expected attribute `key` on tag to be of type string"
    | None, None -> attributes
  in
  let tag =
    Tag.
      { name = tag_name
      ; is_optional
      ; attributes
      ; transformer = apply_transformer ~transformer
      }
  in
  let value =
    match state.tag_info with
    | false -> state |> State.call_tag_listener ~key:("#" ^ tag_name) ~tag
    | true -> `TagInfo tag
  in
  state |> State.add_output ~output:value

and eval_template ~state template =
  match template with
  | Ast.TextTemplateNode text -> state |> State.add_output ~output:(`String text)
  | Ast.HtmlTemplateNode { tag; attributes; children; self_closing } ->
    let attributes =
      attributes
      |> StringMap.map (eval_expression ~state)
      |> StringMap.map State.get_output
    in
    let children =
      children |> List.map (eval_template ~state) |> List.map State.get_output
    in
    state
    |> State.add_output
         ~output:(`TemplateNode (`Html, tag, attributes, children, self_closing))
  | Ast.ExpressionTemplateNode expr -> eval_expression ~state expr
  | Ast.ComponentTemplateNode { identifier = Uppercase_Id tag; attributes; children } ->
    let attributes =
      attributes
      |> StringMap.map (eval_expression ~state)
      |> StringMap.map State.get_output
    in
    let children =
      children |> List.map (eval_template ~state) |> List.map State.get_output
    in
    let component_tag_listeters =
      StringMap.empty
      |> StringMap.add "#String" (StringTag.get_value attributes)
      |> StringMap.add "#Int" (IntTag.get_value attributes)
      |> StringMap.add "#Float" (FloatTag.get_value attributes)
      |> StringMap.add "#Boolean" (BooleanTag.get_value attributes)
      |> StringMap.add "#Array" (ArrayTag.get_value attributes)
      |> StringMap.add "#Record" (RecordTag.get_value attributes)
      |> StringMap.add "#Slot" (SlotTag.get_value children)
    in
    let tag_listeners =
      component_tag_listeters
      |> StringMap.merge
           (fun _key x y ->
             match x, y with
             | None, Some y -> Some y
             | Some x, None -> Some x
             | None, None -> None
             | Some _x, Some y -> Some y)
           state.tag_listeners
    in
    let render_fn ~models ~slotted_children =
      let state =
        State.make ~tag_listeners ~models ~slotted_children state.declarations
      in
      state.declarations
      |> StringMap.find_opt tag
      |> (function
           | Some declaration -> eval_declaration ~state declaration
           | None -> failwith ("Declaration with name `" ^ tag ^ "` was not found."))
      |> State.get_output
    in
    state
    |> State.add_output
         ~output:(`TemplateNode (`Component render_fn, tag, attributes, children, false))

and eval_declaration ~state declaration =
  match declaration with
  | Ast.ComponentDeclaration (_attrs, body)
  | Ast.SiteDeclaration (_attrs, body)
  | Ast.PageDeclaration (_attrs, body)
  | Ast.StoreDeclaration (_attrs, body) -> eval_expression ~state body
;;

let eval ?tag_listeners ?models ?slotted_children ~root declarations =
  let context = Hashtbl.create 10 in
  let tag_listeners =
    let default_tag_listeters =
      StringMap.empty
      |> StringMap.add "#SetContext" (SetContextTag.get_value context)
      |> StringMap.add "#GetContext" (GetContextTag.get_value context)
    in
    match tag_listeners with
    | None -> default_tag_listeters
    | Some listeners ->
      default_tag_listeters
      |> StringMap.merge
           (fun _key x y ->
             match x, y with
             | None, Some y -> Some y
             | Some x, None -> Some x
             | None, None -> None
             | Some x, Some _y -> Some x)
           listeners
  in
  let state = State.make ~tag_listeners ?models ?slotted_children declarations in
  declarations
  |> StringMap.find_opt root
  |> function
  | Some declaration -> eval_declaration ~state declaration
  | None -> failwith ("Declaration with name `" ^ root ^ "` was not found.")
;;

let from_source ?models ?slotted_children ?(filename = "") ~source root =
  let declarations = Parser.parse ~filename source in
  eval ?models ?slotted_children ~root declarations
;;
