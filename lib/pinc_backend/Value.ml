module Diagnostics = Pinc_Diagnostics
include Types.Type_Value

let rec to_string value =
  match value.value_desc with
  | Portal list -> list |> List.rev_map to_string |> String.concat "\n"
  | Null -> ""
  | String s -> s
  | Char c ->
      let buf = Buffer.create 32 in
      c |> Buffer.add_utf_8_uchar buf;
      Buffer.contents buf
  | Int i -> string_of_int i
  | Float f when Float.is_integer f -> string_of_int (int_of_float f)
  | Float f -> string_of_float f
  | Bool b ->
      if b then
        "true"
      else
        "false"
  | Array l ->
      let buf = Buffer.create 200 in
      l
      |> Array.iteri (fun i it ->
             if i <> 0 then
               Buffer.add_char buf '\n';
             Buffer.add_string buf (to_string it));
      Buffer.contents buf
  | Record m ->
      let b = Buffer.create 1024 in
      m
      |> StringMap.to_seq
      |> Seq.iter (fun (_key, value) ->
             Buffer.add_string b (to_string value);
             Buffer.add_char b '\n');
      Buffer.contents b
  | HtmlTemplateNode (tag, attributes, children, self_closing) ->
      let buf = Buffer.create 128 in
      Buffer.add_char buf '<';
      Buffer.add_string buf tag;
      if not (StringMap.is_empty attributes) then
        attributes
        |> StringMap.iter (fun key value ->
               match value.value_desc with
               | Null -> ()
               | Portal _
               | Function _
               | String _
               | Int _
               | Char _
               | Float _
               | Bool _
               | Array _
               | Record _
               | HtmlTemplateNode _
               | ComponentTemplateNode _
               | DefinitionInfo _ ->
                   Buffer.add_char buf ' ';
                   Buffer.add_string buf key;
                   Buffer.add_char buf '=';
                   Buffer.add_char buf '"';
                   Buffer.add_string buf (value |> to_string);
                   Buffer.add_char buf '"');
      if self_closing && HTML.is_void_el tag then
        Buffer.add_string buf " />"
      else (
        Buffer.add_char buf '>';
        children |> List.iter (fun child -> Buffer.add_string buf (to_string child));
        Buffer.add_char buf '<';
        Buffer.add_char buf '/';
        Buffer.add_string buf tag;
        Buffer.add_char buf '>');
      Buffer.contents buf
  | ComponentTemplateNode (_tag, _attributes, result) -> result |> to_string
  | Function _ -> ""
  | DefinitionInfo _ -> ""
;;

let is_true value =
  match value.value_desc with
  | Null -> false
  | Bool b -> b
  | String s -> s |> String.trim |> String.length > 0
  | Char _ -> true
  | Int _ -> true
  | Float _ -> true
  | HtmlTemplateNode _ -> true
  | ComponentTemplateNode _ -> true
  | Portal _ -> false
  | DefinitionInfo (_name, Some _, _negated) -> true
  | DefinitionInfo (_name, None, _negated) -> false
  | Function _ -> true
  | Array [||] -> false
  | Array _ -> true
  | Record m -> not (StringMap.is_empty m)
;;

let rec equal a b =
  match (a.value_desc, b.value_desc) with
  | String a, String b -> String.equal a b
  | Char a, Char b -> Uchar.equal a b
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Float a, Int b -> a = float_of_int b
  | Int a, Float b -> float_of_int a = b
  | Bool a, Bool b -> a = b
  | Array a, Array b -> Array.combine a b |> Array.for_all (fun (a, b) -> equal a b)
  | Record a, Record b -> StringMap.equal equal a b
  | Function _, Function _ -> false
  | DefinitionInfo (a, _, _), DefinitionInfo (b, _, _) -> String.equal a b
  | ( HtmlTemplateNode (a_tag, a_attrs, a_children, a_self_closing),
      HtmlTemplateNode (b_tag, b_attrs, b_children, b_self_closing) ) ->
      a_tag = b_tag
      && a_self_closing = b_self_closing
      && StringMap.equal equal a_attrs b_attrs
      && a_children = b_children
  | ( ComponentTemplateNode (a_tag, a_attributes, _),
      ComponentTemplateNode (b_tag, b_attributes, _) ) ->
      a_tag = b_tag && StringMap.equal equal a_attributes b_attributes
  | Null, Null -> true
  | _ -> false
;;

let compare a b =
  match (a.value_desc, b.value_desc) with
  | String a, String b -> String.compare a b
  | Char a, Char b -> Uchar.compare a b
  | Char a, Int b -> Int.compare (Uchar.to_int a) b
  | Int a, Char b -> Int.compare a (Uchar.to_int b)
  | Int a, Int b -> Int.compare a b
  | Float a, Float b -> Float.compare a b
  | Float a, Int b -> Float.compare a (float_of_int b)
  | Int a, Float b -> Float.compare (float_of_int a) b
  | Bool a, Bool b -> Bool.compare a b
  | Array a, Array b -> Int.compare (Array.length a) (Array.length b)
  | Record a, Record b -> StringMap.compare compare a b
  | Null, Null -> 0
  | ComponentTemplateNode _, ComponentTemplateNode _ -> 0
  | HtmlTemplateNode _, HtmlTemplateNode _ -> 0
  | DefinitionInfo _, DefinitionInfo _ -> 0
  | Function _, Function _ -> 0
  | Portal _, _ -> 0
  | _, Portal _ -> 0
  | _ -> 0
;;

let show value =
  match value.value_desc with
  | Null -> "null"
  | Bool b -> Printf.sprintf "bool (%b)" b
  | String s -> Printf.sprintf "string (%s)" s
  | Char c -> Printf.sprintf "char (%i)" (Uchar.to_int c)
  | Int i -> Printf.sprintf "int (%i)" i
  | Float f -> Printf.sprintf "float (%f)" f
  | HtmlTemplateNode _ -> Printf.sprintf "HtmlTemplateNode"
  | ComponentTemplateNode _ -> Printf.sprintf "ComponentTemplateNode"
  | Portal _ -> Printf.sprintf "Portal"
  | DefinitionInfo (name, _, _) -> Printf.sprintf "Definition (%s)" name
  | Function _ -> Printf.sprintf "function"
  | Array _ -> Printf.sprintf "array"
  | Record _ -> Printf.sprintf "record"
;;
