[@@@ocaml.warning "-27-30-39"]

type t_null = unit

type t_value =
  | V_null
  | V_list of t_list
  | V_record of t_struct
  | V_bool of bool
  | V_int of int32
  | V_float of float
  | V_string of string

and t_list = {
  value : t_value list;
}

and t_struct = {
  value : (string * t_value) list;
}

type string_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type string_response = {
  value : string;
  meta : (string * t_value) list;
}

type float_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type float_response = {
  value : float;
  meta : (string * t_value) list;
}

type int_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type int_response = {
  value : int32;
  meta : (string * t_value) list;
}

type bool_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type bool_response = {
  value : bool;
  meta : (string * t_value) list;
}

type array_request = {
  required : bool;
  key : string;
  of_ : t_tag option;
  attributes : (string * t_value) list;
}

and t_tag =
  | T_list of array_request
  | T_record of record_request
  | T_bool of bool_request
  | T_int of int_request
  | T_float of float_request
  | T_string of string_request

and record_request = {
  required : bool;
  key : string;
  of_ : record_request_t_tag_struct option;
  attributes : (string * t_value) list;
}

and record_request_t_tag_struct = {
  value : record_request_t_tag_struct_t_tag_struct_item list;
}

and record_request_t_tag_struct_t_tag_struct_item = {
  key : string;
  required : bool;
  value : t_tag option;
}

type array_response = {
  value : t_value list;
  meta : (string * t_value) list;
}

type record_response = {
  value : (string * t_value) list;
  meta : (string * t_value) list;
}

let rec default_t_null = ()

let rec default_t_value (): t_value = V_null

and default_t_list 
  ?value:((value:t_value list) = [])
  () : t_list  = {
  value;
}

and default_t_struct 
  ?value:((value:(string * t_value) list) = [])
  () : t_struct  = {
  value;
}

let rec default_string_request 
  ?required:((required:bool) = false)
  ?key:((key:string) = "")
  ?attributes:((attributes:(string * t_value) list) = [])
  () : string_request  = {
  required;
  key;
  attributes;
}

let rec default_string_response 
  ?value:((value:string) = "")
  ?meta:((meta:(string * t_value) list) = [])
  () : string_response  = {
  value;
  meta;
}

let rec default_float_request 
  ?required:((required:bool) = false)
  ?key:((key:string) = "")
  ?attributes:((attributes:(string * t_value) list) = [])
  () : float_request  = {
  required;
  key;
  attributes;
}

let rec default_float_response 
  ?value:((value:float) = 0.)
  ?meta:((meta:(string * t_value) list) = [])
  () : float_response  = {
  value;
  meta;
}

let rec default_int_request 
  ?required:((required:bool) = false)
  ?key:((key:string) = "")
  ?attributes:((attributes:(string * t_value) list) = [])
  () : int_request  = {
  required;
  key;
  attributes;
}

let rec default_int_response 
  ?value:((value:int32) = 0l)
  ?meta:((meta:(string * t_value) list) = [])
  () : int_response  = {
  value;
  meta;
}

let rec default_bool_request 
  ?required:((required:bool) = false)
  ?key:((key:string) = "")
  ?attributes:((attributes:(string * t_value) list) = [])
  () : bool_request  = {
  required;
  key;
  attributes;
}

let rec default_bool_response 
  ?value:((value:bool) = false)
  ?meta:((meta:(string * t_value) list) = [])
  () : bool_response  = {
  value;
  meta;
}

let rec default_array_request 
  ?required:((required:bool) = false)
  ?key:((key:string) = "")
  ?of_:((of_:t_tag option) = None)
  ?attributes:((attributes:(string * t_value) list) = [])
  () : array_request  = {
  required;
  key;
  of_;
  attributes;
}

and default_t_tag () : t_tag = T_list (default_array_request ())

and default_record_request 
  ?required:((required:bool) = false)
  ?key:((key:string) = "")
  ?of_:((of_:record_request_t_tag_struct option) = None)
  ?attributes:((attributes:(string * t_value) list) = [])
  () : record_request  = {
  required;
  key;
  of_;
  attributes;
}

and default_record_request_t_tag_struct 
  ?value:((value:record_request_t_tag_struct_t_tag_struct_item list) = [])
  () : record_request_t_tag_struct  = {
  value;
}

and default_record_request_t_tag_struct_t_tag_struct_item 
  ?key:((key:string) = "")
  ?required:((required:bool) = false)
  ?value:((value:t_tag option) = None)
  () : record_request_t_tag_struct_t_tag_struct_item  = {
  key;
  required;
  value;
}

let rec default_array_response 
  ?value:((value:t_value list) = [])
  ?meta:((meta:(string * t_value) list) = [])
  () : array_response  = {
  value;
  meta;
}

let rec default_record_response 
  ?value:((value:(string * t_value) list) = [])
  ?meta:((meta:(string * t_value) list) = [])
  () : record_response  = {
  value;
  meta;
}

type t_list_mutable = {
  mutable value : t_value list;
}

let default_t_list_mutable () : t_list_mutable = {
  value = [];
}

type t_struct_mutable = {
  mutable value : (string * t_value) list;
}

let default_t_struct_mutable () : t_struct_mutable = {
  value = [];
}

type string_request_mutable = {
  mutable required : bool;
  mutable key : string;
  mutable attributes : (string * t_value) list;
}

let default_string_request_mutable () : string_request_mutable = {
  required = false;
  key = "";
  attributes = [];
}

type string_response_mutable = {
  mutable value : string;
  mutable meta : (string * t_value) list;
}

let default_string_response_mutable () : string_response_mutable = {
  value = "";
  meta = [];
}

type float_request_mutable = {
  mutable required : bool;
  mutable key : string;
  mutable attributes : (string * t_value) list;
}

let default_float_request_mutable () : float_request_mutable = {
  required = false;
  key = "";
  attributes = [];
}

type float_response_mutable = {
  mutable value : float;
  mutable meta : (string * t_value) list;
}

let default_float_response_mutable () : float_response_mutable = {
  value = 0.;
  meta = [];
}

type int_request_mutable = {
  mutable required : bool;
  mutable key : string;
  mutable attributes : (string * t_value) list;
}

let default_int_request_mutable () : int_request_mutable = {
  required = false;
  key = "";
  attributes = [];
}

type int_response_mutable = {
  mutable value : int32;
  mutable meta : (string * t_value) list;
}

let default_int_response_mutable () : int_response_mutable = {
  value = 0l;
  meta = [];
}

type bool_request_mutable = {
  mutable required : bool;
  mutable key : string;
  mutable attributes : (string * t_value) list;
}

let default_bool_request_mutable () : bool_request_mutable = {
  required = false;
  key = "";
  attributes = [];
}

type bool_response_mutable = {
  mutable value : bool;
  mutable meta : (string * t_value) list;
}

let default_bool_response_mutable () : bool_response_mutable = {
  value = false;
  meta = [];
}

type array_request_mutable = {
  mutable required : bool;
  mutable key : string;
  mutable of_ : t_tag option;
  mutable attributes : (string * t_value) list;
}

let default_array_request_mutable () : array_request_mutable = {
  required = false;
  key = "";
  of_ = None;
  attributes = [];
}

type record_request_mutable = {
  mutable required : bool;
  mutable key : string;
  mutable of_ : record_request_t_tag_struct option;
  mutable attributes : (string * t_value) list;
}

let default_record_request_mutable () : record_request_mutable = {
  required = false;
  key = "";
  of_ = None;
  attributes = [];
}

type record_request_t_tag_struct_mutable = {
  mutable value : record_request_t_tag_struct_t_tag_struct_item list;
}

let default_record_request_t_tag_struct_mutable () : record_request_t_tag_struct_mutable = {
  value = [];
}

type record_request_t_tag_struct_t_tag_struct_item_mutable = {
  mutable key : string;
  mutable required : bool;
  mutable value : t_tag option;
}

let default_record_request_t_tag_struct_t_tag_struct_item_mutable () : record_request_t_tag_struct_t_tag_struct_item_mutable = {
  key = "";
  required = false;
  value = None;
}

type array_response_mutable = {
  mutable value : t_value list;
  mutable meta : (string * t_value) list;
}

let default_array_response_mutable () : array_response_mutable = {
  value = [];
  meta = [];
}

type record_response_mutable = {
  mutable value : (string * t_value) list;
  mutable meta : (string * t_value) list;
}

let default_record_response_mutable () : record_response_mutable = {
  value = [];
  meta = [];
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_t_null (v:t_null) encoder = 
()

let rec encode_pb_t_value (v:t_value) encoder = 
  begin match v with
  | V_null ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | V_list x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_t_list x) encoder;
  | V_record x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_t_struct x) encoder;
  | V_bool x ->
    Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
    Pbrt.Encoder.bool x encoder;
  | V_int x ->
    Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int32_as_varint x encoder;
  | V_float x ->
    Pbrt.Encoder.key (3, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | V_string x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  end

and encode_pb_t_list (v:t_list) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) v.value;
  ()

and encode_pb_t_struct (v:t_struct) encoder = 
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.value;
  ()

let rec encode_pb_string_request (v:string_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  ()

let rec encode_pb_string_response (v:string_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.value encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.meta;
  ()

let rec encode_pb_float_request (v:float_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  ()

let rec encode_pb_float_response (v:float_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.value encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.meta;
  ()

let rec encode_pb_int_request (v:int_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  ()

let rec encode_pb_int_response (v:int_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.value encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.meta;
  ()

let rec encode_pb_bool_request (v:bool_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  ()

let rec encode_pb_bool_response (v:bool_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.value encoder;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.meta;
  ()

let rec encode_pb_array_request (v:array_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  begin match v.of_ with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_t_tag x) encoder;
  | None -> ();
  end;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  ()

and encode_pb_t_tag (v:t_tag) encoder = 
  begin match v with
  | T_list x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_array_request x) encoder;
  | T_record x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_record_request x) encoder;
  | T_bool x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_bool_request x) encoder;
  | T_int x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_int_request x) encoder;
  | T_float x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_float_request x) encoder;
  | T_string x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_string_request x) encoder;
  end

and encode_pb_record_request (v:record_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  begin match v.of_ with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_record_request_t_tag_struct x) encoder;
  | None -> ();
  end;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  ()

and encode_pb_record_request_t_tag_struct (v:record_request_t_tag_struct) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_record_request_t_tag_struct_t_tag_struct_item x) encoder;
  ) v.value;
  ()

and encode_pb_record_request_t_tag_struct_t_tag_struct_item (v:record_request_t_tag_struct_t_tag_struct_item) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.key encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  begin match v.value with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_t_tag x) encoder;
  | None -> ();
  end;
  ()

let rec encode_pb_array_response (v:array_response) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) v.value;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.meta;
  ()

let rec encode_pb_record_response (v:record_response) encoder = 
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.value;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.meta;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_t_null d =
  match Pbrt.Decoder.key d with
  | None -> ();
  | Some (_, pk) -> 
    Pbrt.Decoder.unexpected_payload "Unexpected fields in empty message(t_null)" pk

let rec decode_pb_t_value d = 
  let rec loop () = 
    let ret:t_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "t_value"
      | Some (8, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (V_null : t_value)
      end
      | Some (7, _) -> (V_list (decode_pb_t_list (Pbrt.Decoder.nested d)) : t_value) 
      | Some (6, _) -> (V_record (decode_pb_t_struct (Pbrt.Decoder.nested d)) : t_value) 
      | Some (5, _) -> (V_bool (Pbrt.Decoder.bool d) : t_value) 
      | Some (4, _) -> (V_int (Pbrt.Decoder.int32_as_varint d) : t_value) 
      | Some (3, _) -> (V_float (Pbrt.Decoder.float_as_bits32 d) : t_value) 
      | Some (2, _) -> (V_string (Pbrt.Decoder.string d) : t_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_t_list d =
  let v = default_t_list_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.value <- List.rev v.value;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- (decode_pb_t_value (Pbrt.Decoder.nested d)) :: v.value;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(t_list), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
  } : t_list)

and decode_pb_t_struct d =
  let v = default_t_struct_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.value <- List.rev v.value;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.value <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.value;
      );
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(t_struct), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
  } : t_struct)

let rec decode_pb_string_request d =
  let v = default_string_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_request), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    attributes = v.attributes;
  } : string_request)

let rec decode_pb_string_response d =
  let v = default_string_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.meta <- List.rev v.meta;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.meta;
      );
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_response), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
    meta = v.meta;
  } : string_response)

let rec decode_pb_float_request d =
  let v = default_float_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(float_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(float_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(float_request), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    attributes = v.attributes;
  } : float_request)

let rec decode_pb_float_response d =
  let v = default_float_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.meta <- List.rev v.meta;
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.value <- Pbrt.Decoder.float_as_bits32 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(float_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.meta;
      );
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(float_response), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
    meta = v.meta;
  } : float_response)

let rec decode_pb_int_request d =
  let v = default_int_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(int_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(int_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(int_request), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    attributes = v.attributes;
  } : int_request)

let rec decode_pb_int_response d =
  let v = default_int_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.meta <- List.rev v.meta;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.value <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(int_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.meta;
      );
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(int_response), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
    meta = v.meta;
  } : int_response)

let rec decode_pb_bool_request d =
  let v = default_bool_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bool_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bool_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bool_request), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    attributes = v.attributes;
  } : bool_request)

let rec decode_pb_bool_response d =
  let v = default_bool_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.meta <- List.rev v.meta;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.value <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bool_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.meta;
      );
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bool_response), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
    meta = v.meta;
  } : bool_response)

let rec decode_pb_array_request d =
  let v = default_array_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.of_ <- Some (decode_pb_t_tag (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_request), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_request), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    of_ = v.of_;
    attributes = v.attributes;
  } : array_request)

and decode_pb_t_tag d = 
  let rec loop () = 
    let ret:t_tag = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "t_tag"
      | Some (7, _) -> (T_list (decode_pb_array_request (Pbrt.Decoder.nested d)) : t_tag) 
      | Some (6, _) -> (T_record (decode_pb_record_request (Pbrt.Decoder.nested d)) : t_tag) 
      | Some (5, _) -> (T_bool (decode_pb_bool_request (Pbrt.Decoder.nested d)) : t_tag) 
      | Some (4, _) -> (T_int (decode_pb_int_request (Pbrt.Decoder.nested d)) : t_tag) 
      | Some (3, _) -> (T_float (decode_pb_float_request (Pbrt.Decoder.nested d)) : t_tag) 
      | Some (2, _) -> (T_string (decode_pb_string_request (Pbrt.Decoder.nested d)) : t_tag) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_record_request d =
  let v = default_record_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.of_ <- Some (decode_pb_record_request_t_tag_struct (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    of_ = v.of_;
    attributes = v.attributes;
  } : record_request)

and decode_pb_record_request_t_tag_struct d =
  let v = default_record_request_t_tag_struct_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.value <- List.rev v.value;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- (decode_pb_record_request_t_tag_struct_t_tag_struct_item (Pbrt.Decoder.nested d)) :: v.value;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request_t_tag_struct), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
  } : record_request_t_tag_struct)

and decode_pb_record_request_t_tag_struct_t_tag_struct_item d =
  let v = default_record_request_t_tag_struct_t_tag_struct_item_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request_t_tag_struct_t_tag_struct_item), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request_t_tag_struct_t_tag_struct_item), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.value <- Some (decode_pb_t_tag (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_request_t_tag_struct_t_tag_struct_item), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    key = v.key;
    required = v.required;
    value = v.value;
  } : record_request_t_tag_struct_t_tag_struct_item)

let rec decode_pb_array_response d =
  let v = default_array_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.meta <- List.rev v.meta;
      v.value <- List.rev v.value;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- (decode_pb_t_value (Pbrt.Decoder.nested d)) :: v.value;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.meta;
      );
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_response), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
    meta = v.meta;
  } : array_response)

let rec decode_pb_record_response d =
  let v = default_record_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.meta <- List.rev v.meta;
      v.value <- List.rev v.value;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.value <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.value;
      );
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.meta;
      );
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(record_response), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    value = v.value;
    meta = v.meta;
  } : record_response)
