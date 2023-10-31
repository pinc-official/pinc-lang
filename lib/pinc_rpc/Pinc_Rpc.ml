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
    ~net
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    (host, port) =
  let payload = Definitions.default_string_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_string_request payload encoder in
  let payload = Pbrt.Encoder.to_string encoder in
  let decode = function
    | Some response ->
        response |> Pbrt.Decoder.of_string |> Definitions.decode_pb_string_response
    | None -> Definitions.default_string_response ()
  in
  Eio.Switch.run @@ fun sw ->
  let inet, port =
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ])
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
  let socket = Eio.Net.connect ~sw net addr in
  let connection = H2_eio.Client.create_connection ~sw ~error_handler:ignore socket in
  let result =
    Grpc_eio.Client.call
      ~service:"pinc.Tags"
      ~rpc:"string"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:(Grpc_eio.Client.Rpc.unary payload ~f:decode)
      ()
  in
  Eio.Promise.await (H2_eio.Client.shutdown connection);
  match result with
  | Ok (response, _) -> response.value
  | Error _ -> (Definitions.default_string_response ()).value
;;

let make_int_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_int_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_int_request payload encoder in
  Pbrt.Encoder.to_string encoder
;;

let make_float_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_float_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_float_request payload encoder in
  Pbrt.Encoder.to_string encoder
;;

let make_bool_request
    ?(required = false)
    ?(attributes : (string * Value.t) list = [])
    ~key
    () =
  let payload = Definitions.default_bool_request ~required ~key ~attributes () in
  let encoder = Pbrt.Encoder.create () in
  let () = Definitions.encode_pb_bool_request payload encoder in
  Pbrt.Encoder.to_string encoder
;;
