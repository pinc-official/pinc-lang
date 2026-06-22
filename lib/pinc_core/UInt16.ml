module T = struct
  include Int

  let make i =
    if i > 65535 then
      raise (Invalid_argument "UInt16.make")
    else
      i
  ;;

  let to_int t = t
  let width _ = 2
  let max_value = 65535

  let write bytes offset t =
    Bytes.set_uint16_be bytes offset t;
    offset + width t
  ;;

  let read bytes offset =
    let result = Bytes.get_uint16_be bytes offset in
    (offset + width result, result)
  ;;

  let incr = incr
  let pp fmt t = Format.fprintf fmt "0x%04X (%04i)" t t
end

include T
module Map = Map.Make (T)
