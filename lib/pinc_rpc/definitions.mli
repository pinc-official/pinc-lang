
(** Code for definitions.proto *)

(* generated from "definitions.proto", do not edit *)



(** {2 Types} *)

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


(** {2 Basic values} *)

val default_t_null : unit
(** [default_t_null ()] is the default value for type [t_null] *)

val default_t_value : unit -> t_value
(** [default_t_value ()] is the default value for type [t_value] *)

val default_t_list : 
  ?value:t_value list ->
  unit ->
  t_list
(** [default_t_list ()] is the default value for type [t_list] *)

val default_t_struct : 
  ?value:(string * t_value) list ->
  unit ->
  t_struct
(** [default_t_struct ()] is the default value for type [t_struct] *)

val default_tag_request : 
  ?required:bool ->
  ?key:string list ->
  ?attributes:(string * t_value) list ->
  ?child_meta:(string * t_value) list ->
  unit ->
  tag_request
(** [default_tag_request ()] is the default value for type [tag_request] *)

val default_string_response : 
  ?value:string ->
  ?meta:(string * t_value) list ->
  unit ->
  string_response
(** [default_string_response ()] is the default value for type [string_response] *)

val default_float_response : 
  ?value:float ->
  ?meta:(string * t_value) list ->
  unit ->
  float_response
(** [default_float_response ()] is the default value for type [float_response] *)

val default_int_response : 
  ?value:int32 ->
  ?meta:(string * t_value) list ->
  unit ->
  int_response
(** [default_int_response ()] is the default value for type [int_response] *)

val default_bool_response : 
  ?value:bool ->
  ?meta:(string * t_value) list ->
  unit ->
  bool_response
(** [default_bool_response ()] is the default value for type [bool_response] *)

val default_array_response : 
  ?value:t_value list ->
  ?meta:(string * t_value) list ->
  unit ->
  array_response
(** [default_array_response ()] is the default value for type [array_response] *)

val default_record_response : 
  ?value:(string * t_value) list ->
  ?meta:(string * t_value) list ->
  unit ->
  record_response
(** [default_record_response ()] is the default value for type [record_response] *)


(** {2 Protobuf Encoding} *)

val encode_pb_t_null : t_null -> Pbrt.Encoder.t -> unit
(** [encode_pb_t_null v encoder] encodes [v] with the given [encoder] *)

val encode_pb_t_value : t_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_t_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_t_list : t_list -> Pbrt.Encoder.t -> unit
(** [encode_pb_t_list v encoder] encodes [v] with the given [encoder] *)

val encode_pb_t_struct : t_struct -> Pbrt.Encoder.t -> unit
(** [encode_pb_t_struct v encoder] encodes [v] with the given [encoder] *)

val encode_pb_tag_request : tag_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_tag_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_string_response : string_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_string_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_float_response : float_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_float_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_int_response : int_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_int_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_bool_response : bool_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_bool_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_array_response : array_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_array_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_record_response : record_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_record_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_t_null : Pbrt.Decoder.t -> t_null
(** [decode_pb_t_null decoder] decodes a [t_null] binary value from [decoder] *)

val decode_pb_t_value : Pbrt.Decoder.t -> t_value
(** [decode_pb_t_value decoder] decodes a [t_value] binary value from [decoder] *)

val decode_pb_t_list : Pbrt.Decoder.t -> t_list
(** [decode_pb_t_list decoder] decodes a [t_list] binary value from [decoder] *)

val decode_pb_t_struct : Pbrt.Decoder.t -> t_struct
(** [decode_pb_t_struct decoder] decodes a [t_struct] binary value from [decoder] *)

val decode_pb_tag_request : Pbrt.Decoder.t -> tag_request
(** [decode_pb_tag_request decoder] decodes a [tag_request] binary value from [decoder] *)

val decode_pb_string_response : Pbrt.Decoder.t -> string_response
(** [decode_pb_string_response decoder] decodes a [string_response] binary value from [decoder] *)

val decode_pb_float_response : Pbrt.Decoder.t -> float_response
(** [decode_pb_float_response decoder] decodes a [float_response] binary value from [decoder] *)

val decode_pb_int_response : Pbrt.Decoder.t -> int_response
(** [decode_pb_int_response decoder] decodes a [int_response] binary value from [decoder] *)

val decode_pb_bool_response : Pbrt.Decoder.t -> bool_response
(** [decode_pb_bool_response decoder] decodes a [bool_response] binary value from [decoder] *)

val decode_pb_array_response : Pbrt.Decoder.t -> array_response
(** [decode_pb_array_response decoder] decodes a [array_response] binary value from [decoder] *)

val decode_pb_record_response : Pbrt.Decoder.t -> record_response
(** [decode_pb_record_response decoder] decodes a [record_response] binary value from [decoder] *)
