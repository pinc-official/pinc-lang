
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

type string_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type string_response = {
  value : string;
}

type float_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type float_response = {
  value : float;
}

type int_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type int_response = {
  value : int32;
}

type bool_request = {
  required : bool;
  key : string;
  attributes : (string * t_value) list;
}

type bool_response = {
  value : bool;
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
}

type record_response = {
  value : (string * t_value) list;
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

val default_string_request : 
  ?required:bool ->
  ?key:string ->
  ?attributes:(string * t_value) list ->
  unit ->
  string_request
(** [default_string_request ()] is the default value for type [string_request] *)

val default_string_response : 
  ?value:string ->
  unit ->
  string_response
(** [default_string_response ()] is the default value for type [string_response] *)

val default_float_request : 
  ?required:bool ->
  ?key:string ->
  ?attributes:(string * t_value) list ->
  unit ->
  float_request
(** [default_float_request ()] is the default value for type [float_request] *)

val default_float_response : 
  ?value:float ->
  unit ->
  float_response
(** [default_float_response ()] is the default value for type [float_response] *)

val default_int_request : 
  ?required:bool ->
  ?key:string ->
  ?attributes:(string * t_value) list ->
  unit ->
  int_request
(** [default_int_request ()] is the default value for type [int_request] *)

val default_int_response : 
  ?value:int32 ->
  unit ->
  int_response
(** [default_int_response ()] is the default value for type [int_response] *)

val default_bool_request : 
  ?required:bool ->
  ?key:string ->
  ?attributes:(string * t_value) list ->
  unit ->
  bool_request
(** [default_bool_request ()] is the default value for type [bool_request] *)

val default_bool_response : 
  ?value:bool ->
  unit ->
  bool_response
(** [default_bool_response ()] is the default value for type [bool_response] *)

val default_array_request : 
  ?required:bool ->
  ?key:string ->
  ?of_:t_tag option ->
  ?attributes:(string * t_value) list ->
  unit ->
  array_request
(** [default_array_request ()] is the default value for type [array_request] *)

val default_t_tag : unit -> t_tag
(** [default_t_tag ()] is the default value for type [t_tag] *)

val default_record_request : 
  ?required:bool ->
  ?key:string ->
  ?of_:record_request_t_tag_struct option ->
  ?attributes:(string * t_value) list ->
  unit ->
  record_request
(** [default_record_request ()] is the default value for type [record_request] *)

val default_record_request_t_tag_struct : 
  ?value:record_request_t_tag_struct_t_tag_struct_item list ->
  unit ->
  record_request_t_tag_struct
(** [default_record_request_t_tag_struct ()] is the default value for type [record_request_t_tag_struct] *)

val default_record_request_t_tag_struct_t_tag_struct_item : 
  ?key:string ->
  ?required:bool ->
  ?value:t_tag option ->
  unit ->
  record_request_t_tag_struct_t_tag_struct_item
(** [default_record_request_t_tag_struct_t_tag_struct_item ()] is the default value for type [record_request_t_tag_struct_t_tag_struct_item] *)

val default_array_response : 
  ?value:t_value list ->
  unit ->
  array_response
(** [default_array_response ()] is the default value for type [array_response] *)

val default_record_response : 
  ?value:(string * t_value) list ->
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

val encode_pb_string_request : string_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_string_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_string_response : string_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_string_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_float_request : float_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_float_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_float_response : float_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_float_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_int_request : int_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_int_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_int_response : int_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_int_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_bool_request : bool_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_bool_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_bool_response : bool_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_bool_response v encoder] encodes [v] with the given [encoder] *)

val encode_pb_array_request : array_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_array_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_t_tag : t_tag -> Pbrt.Encoder.t -> unit
(** [encode_pb_t_tag v encoder] encodes [v] with the given [encoder] *)

val encode_pb_record_request : record_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_record_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_record_request_t_tag_struct : record_request_t_tag_struct -> Pbrt.Encoder.t -> unit
(** [encode_pb_record_request_t_tag_struct v encoder] encodes [v] with the given [encoder] *)

val encode_pb_record_request_t_tag_struct_t_tag_struct_item : record_request_t_tag_struct_t_tag_struct_item -> Pbrt.Encoder.t -> unit
(** [encode_pb_record_request_t_tag_struct_t_tag_struct_item v encoder] encodes [v] with the given [encoder] *)

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

val decode_pb_string_request : Pbrt.Decoder.t -> string_request
(** [decode_pb_string_request decoder] decodes a [string_request] binary value from [decoder] *)

val decode_pb_string_response : Pbrt.Decoder.t -> string_response
(** [decode_pb_string_response decoder] decodes a [string_response] binary value from [decoder] *)

val decode_pb_float_request : Pbrt.Decoder.t -> float_request
(** [decode_pb_float_request decoder] decodes a [float_request] binary value from [decoder] *)

val decode_pb_float_response : Pbrt.Decoder.t -> float_response
(** [decode_pb_float_response decoder] decodes a [float_response] binary value from [decoder] *)

val decode_pb_int_request : Pbrt.Decoder.t -> int_request
(** [decode_pb_int_request decoder] decodes a [int_request] binary value from [decoder] *)

val decode_pb_int_response : Pbrt.Decoder.t -> int_response
(** [decode_pb_int_response decoder] decodes a [int_response] binary value from [decoder] *)

val decode_pb_bool_request : Pbrt.Decoder.t -> bool_request
(** [decode_pb_bool_request decoder] decodes a [bool_request] binary value from [decoder] *)

val decode_pb_bool_response : Pbrt.Decoder.t -> bool_response
(** [decode_pb_bool_response decoder] decodes a [bool_response] binary value from [decoder] *)

val decode_pb_array_request : Pbrt.Decoder.t -> array_request
(** [decode_pb_array_request decoder] decodes a [array_request] binary value from [decoder] *)

val decode_pb_t_tag : Pbrt.Decoder.t -> t_tag
(** [decode_pb_t_tag decoder] decodes a [t_tag] binary value from [decoder] *)

val decode_pb_record_request : Pbrt.Decoder.t -> record_request
(** [decode_pb_record_request decoder] decodes a [record_request] binary value from [decoder] *)

val decode_pb_record_request_t_tag_struct : Pbrt.Decoder.t -> record_request_t_tag_struct
(** [decode_pb_record_request_t_tag_struct decoder] decodes a [record_request_t_tag_struct] binary value from [decoder] *)

val decode_pb_record_request_t_tag_struct_t_tag_struct_item : Pbrt.Decoder.t -> record_request_t_tag_struct_t_tag_struct_item
(** [decode_pb_record_request_t_tag_struct_t_tag_struct_item decoder] decodes a [record_request_t_tag_struct_t_tag_struct_item] binary value from [decoder] *)

val decode_pb_array_response : Pbrt.Decoder.t -> array_response
(** [decode_pb_array_response decoder] decodes a [array_response] binary value from [decoder] *)

val decode_pb_record_response : Pbrt.Decoder.t -> record_response
(** [decode_pb_record_response decoder] decodes a [record_response] binary value from [decoder] *)
