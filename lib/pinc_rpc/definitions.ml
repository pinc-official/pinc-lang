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

type tag_request = {
  required : bool;
  key : string list;
  attributes : (string * t_value) list;
  child_meta : (string * t_value) list;
}

type string_response = {
  value : string;
  meta : (string * t_value) list;
}

type float_response = {
  value : float;
  meta : (string * t_value) list;
}

type int_response = {
  value : int32;
  meta : (string * t_value) list;
}

type bool_response = {
  value : bool;
  meta : (string * t_value) list;
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

let rec default_tag_request 
  ?required:((required:bool) = false)
  ?key:((key:string list) = [])
  ?attributes:((attributes:(string * t_value) list) = [])
  ?child_meta:((child_meta:(string * t_value) list) = [])
  () : tag_request  = {
  required;
  key;
  attributes;
  child_meta;
}

let rec default_string_response 
  ?value:((value:string) = "")
  ?meta:((meta:(string * t_value) list) = [])
  () : string_response  = {
  value;
  meta;
}

let rec default_float_response 
  ?value:((value:float) = 0.)
  ?meta:((meta:(string * t_value) list) = [])
  () : float_response  = {
  value;
  meta;
}

let rec default_int_response 
  ?value:((value:int32) = 0l)
  ?meta:((meta:(string * t_value) list) = [])
  () : int_response  = {
  value;
  meta;
}

let rec default_bool_response 
  ?value:((value:bool) = false)
  ?meta:((meta:(string * t_value) list) = [])
  () : bool_response  = {
  value;
  meta;
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

type tag_request_mutable = {
  mutable required : bool;
  mutable key : string list;
  mutable attributes : (string * t_value) list;
  mutable child_meta : (string * t_value) list;
}

let default_tag_request_mutable () : tag_request_mutable = {
  required = false;
  key = [];
  attributes = [];
  child_meta = [];
}

type string_response_mutable = {
  mutable value : string;
  mutable meta : (string * t_value) list;
}

let default_string_response_mutable () : string_response_mutable = {
  value = "";
  meta = [];
}

type float_response_mutable = {
  mutable value : float;
  mutable meta : (string * t_value) list;
}

let default_float_response_mutable () : float_response_mutable = {
  value = 0.;
  meta = [];
}

type int_response_mutable = {
  mutable value : int32;
  mutable meta : (string * t_value) list;
}

let default_int_response_mutable () : int_response_mutable = {
  value = 0l;
  meta = [];
}

type bool_response_mutable = {
  mutable value : bool;
  mutable meta : (string * t_value) list;
}

let default_bool_response_mutable () : bool_response_mutable = {
  value = false;
  meta = [];
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

let rec encode_pb_tag_request (v:tag_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.required encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.key;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.attributes;
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_pb_t_value x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.child_meta;
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

let rec decode_pb_tag_request d =
  let v = default_tag_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.child_meta <- List.rev v.child_meta;
      v.attributes <- List.rev v.attributes;
      v.key <- List.rev v.key;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.required <- Pbrt.Decoder.bool d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tag_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.key <- (Pbrt.Decoder.string d) :: v.key;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tag_request), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.attributes <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.attributes;
      );
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tag_request), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_pb_t_value (Pbrt.Decoder.nested d)
      ) in
      v.child_meta <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.child_meta;
      );
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tag_request), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    required = v.required;
    key = v.key;
    attributes = v.attributes;
    child_meta = v.child_meta;
  } : tag_request)

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
