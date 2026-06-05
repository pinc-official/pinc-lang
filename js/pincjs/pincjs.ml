open Js_of_ocaml
open Pinc_Core
open Pinc_Backend

type encoded =
  [ `Assoc of (string * encoded) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Array of encoded array
  | `Null
  | `String of string
  ]

let is_int s : bool =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Number.isInteger") [| Js.Unsafe.inject s |]
;;

let is_array s : bool =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Array.isArray") [| Js.Unsafe.inject s |]
;;

let rec js_encode (data : 'a Js.Opt.t) : encoded =
  let opt_js_type = Js.Opt.map data (fun d -> `Data d) in
  let js_type = Js.Opt.get opt_js_type (fun () -> `Null) in
  match js_type with
  | `Null -> `Null
  | `Data data -> (
      match Js.to_string @@ Js.typeof data with
      | "undefined" -> `Null
      | "boolean" -> `Bool (Js.to_bool (Js.Unsafe.coerce data))
      | "number" ->
          let n = Js.Unsafe.coerce data in
          if is_int n then
            `Int (Obj.magic n)
          else
            `Float (Js.to_float n)
      | "string" -> `String (Js.to_string (Js.Unsafe.coerce data))
      | "object" when is_array data ->
          let array = Js.to_array (Js.Unsafe.coerce data) in
          let array = Array.map js_encode array in
          `Array array
      | "object" ->
          let obj = Js.Unsafe.coerce data in
          let values =
            Js.object_keys obj
            |> Js.array_map (fun key ->
                let key = Js.to_string key in
                (key, js_encode @@ Js.Unsafe.get obj key))
            |> Js.to_array
            |> Array.to_list
          in
          `Assoc values
      | _ -> `Null)
;;

let rec encoded_to_pinc_value (encoded : encoded) =
  match encoded with
  | `Array a -> a |> Array.map encoded_to_pinc_value |> Helpers.Value.array
  | `Bool b -> Helpers.Value.bool b
  | `Int i -> Helpers.Value.int i
  | `Float f -> Helpers.Value.float f
  | `Null -> Helpers.Value.null ()
  | `Assoc assoc ->
      assoc
      |> StringMap.of_list
      |> StringMap.map encoded_to_pinc_value
      |> Helpers.Value.record
  | `String s -> Helpers.Value.string s
;;

let find_path path (encoded : encoded) =
  path
  |> List.fold_left
       (fun acc segment ->
         match acc with
         | Some (`Array a) -> (
             try Some (Array.get a (int_of_string segment))
             with Invalid_argument _ | Failure _ -> None)
         | Some (`Assoc obj) -> obj |> List.assoc_opt segment
         | _ -> None)
       (Some encoded)
;;

class type inputObj = object
  method name : Js.js_string Js.t Js.readonly_prop
  method code : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  method data : Js.Unsafe.any Js.Opt.t Js.readonly_prop
  method defaultSlot : inputObj Js.t Js.js_array Js.t Js.Optdef.t Js.readonly_prop
end

let get_declarations (input : encoded) =
  match find_path [ "code" ] input with
  | Some (`Array a) ->
      a
      |> Array.fold_left
           (fun acc ->
             (function
             | `String code ->
                 let map = code |> String.of_hex |> Pinc_Types.Ast.unmarshal in
                 StringMap.union (fun _ _ b -> Some b) map acc
             | _ -> acc))
           StringMap.empty
  | _ -> StringMap.empty
;;

let rec eval_slot input data path make_component =
  let build_slot (slot : encoded) =
    let ( let* ) = Option.bind in
    match slot with
    | `Array a ->
        a
        |> Array.to_list
        |> List.filter_map (fun (input : encoded) ->
            let* tag =
              find_path [ "name" ] input |> function
              | Some (`String s) -> Some s
              | _ -> None
            in
            let data =
              find_path [ "data" ] input |> function
              | Some data -> data
              | None -> `Null
            in
            let additional_declarations = get_declarations input in
            let tag_data_provider = tag_data_provider input data in
            make_component
              ~tag
              ?additional_declarations:(Some additional_declarations)
              ?tag_meta_provider:None
              ~tag_data_provider
              ()
            |> snd
            |> Option.some)
        |> Helpers.Value.list
        |> Option.some
    | _ -> None
  in
  match path with
  | [] | [ "" ] -> Option.bind (find_path [ "defaultSlot" ] input) build_slot
  | path ->
      let slot_data = data |> find_path path in
      Option.bind slot_data build_slot

and tag_data_provider input data ~tag ~attributes:_ ~required:_ ~key =
  match (tag : Interpreter.Types.Type_Tag.kind) with
  | Tag_Slot make_component -> eval_slot input data key make_component
  | Tag_Array ->
      Option.bind (find_path key data) (function
        | `Array a ->
            a
            |> Array.mapi (fun idx _ -> Helpers.Value.string (string_of_int idx))
            |> Helpers.Value.array
            |> Option.some
        | _ -> None)
  | Tag_String
  | Tag_Int
  | Tag_Float
  | Tag_Boolean
  | Tag_Record
  | Tag_Custom _
  | Tag_Store _ -> data |> find_path key |> Option.map encoded_to_pinc_value
;;

let pinc_eval (input : inputObj Js.t) =
  let root = Js.to_string input##.name in
  let data = js_encode input##.data in

  let encoded_input = js_encode (Js.Opt.return input) in
  let declarations = get_declarations encoded_input in
  let tag_data_provider = tag_data_provider encoded_input data in

  try
    fst
    @@ Pinc_Backend.Interpreter.eval_declarations declarations ~tag_data_provider ~root
  with Pinc_Diagnostics.Pinc_error s ->
    let error = new%js Js.error_constr (Js.string s) in
    Js_error.raise_ (Js.Js_error.of_error error)
;;

let () = Js.export "run" pinc_eval
