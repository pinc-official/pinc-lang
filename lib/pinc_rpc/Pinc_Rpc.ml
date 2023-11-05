module Client = Pinc_Rpc_Client
module Definitions = Definitions

module Value = struct
  open Definitions

  type t = t_value

  let null = V_null
  let string s = V_string s
  let float f = V_float f
  let int i = V_int (Int32.of_int i)
  let bool b = V_bool b
  let record s = V_record (default_t_struct ~value:s ())
  let list l = V_list (default_t_list ~value:l ())
end

let make_string_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_string_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_string_request payload encoder in
  let decode s = Pbrt.Decoder.of_string s |> Definitions.decode_pb_string_response in
  (Pbrt.Encoder.to_string encoder, decode)
;;

let make_int_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_int_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_int_request payload encoder in
  let decode s = Pbrt.Decoder.of_string s |> Definitions.decode_pb_int_response in
  (Pbrt.Encoder.to_string encoder, decode)
;;

let make_float_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_float_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_float_request payload encoder in
  let decode s = Pbrt.Decoder.of_string s |> Definitions.decode_pb_float_response in
  (Pbrt.Encoder.to_string encoder, decode)
;;

let make_bool_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_bool_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_bool_request payload encoder in
  let decode s = Pbrt.Decoder.of_string s |> Definitions.decode_pb_bool_response in
  (Pbrt.Encoder.to_string encoder, decode)
;;
