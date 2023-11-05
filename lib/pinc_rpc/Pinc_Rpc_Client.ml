let make_address ?(host = "") port =
  let address_info =
    Eio_unix.run_in_systhread @@ fun () ->
    Unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  let inet, port =
    address_info
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port)
;;

let send ~env ~address ~rpc (payload, decoder) =
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.connect ~sw net address in
  let connection = H2_eio.Client.create_connection ~sw ~error_handler:ignore socket in
  let result =
    Grpc_eio.Client.call
      ~service:"pinc.Tags"
      ~rpc
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:(Grpc_eio.Client.Rpc.unary payload ~f:Fun.id)
      ()
  in
  Eio.Promise.await (H2_eio.Client.shutdown connection);
  let response =
    match result with
    | Ok (response, _) -> response
    | Error _ -> None
  in
  response |> Option.map decoder
;;
