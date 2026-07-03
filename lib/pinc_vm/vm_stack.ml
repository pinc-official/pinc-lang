exception Pinc_stack_overflow

type 'a t = {
  stack_size : int;
  mutable stack_pointer : int;
  mutable stack : Pinc_Bytecode.Value.t Array.t;
}

let make ~size =
  {
    stack_size = size;
    stack_pointer = 0;
    stack = Array.make size Pinc_Bytecode.Value.Null;
  }
;;

let set_pointer t n =
  if n > t.stack_size then
    raise_notrace Pinc_stack_overflow
  else
    t.stack_pointer <- n
;;

let push t value =
  if t.stack_pointer > t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    t.stack.(t.stack_pointer) <- value;
    t.stack_pointer <- succ t.stack_pointer)
;;

let pop t =
  if t.stack_pointer == 0 then
    assert false
  else (
    let value = t.stack.(t.stack_pointer - 1) in
    t.stack_pointer <- pred t.stack_pointer;
    value)
;;

let pop_n t n =
  if t.stack_pointer < n then
    assert false
  else (
    let elements = List.init n (fun index -> t.stack.(t.stack_pointer - n + index)) in
    t.stack_pointer <- t.stack_pointer - n;
    elements)
;;

let nth t n =
  match t.stack_pointer - n with
  | 0 -> Pinc_Bytecode.Value.Null
  | n -> t.stack.(n - 1)
;;

let top t = nth t 0

let set t address value =
  if address > t.stack_size then
    raise_notrace Pinc_stack_overflow
  else
    t.stack.(address) <- value
;;

let get t address =
  if address > t.stack_size then
    raise_notrace Pinc_stack_overflow
  else
    t.stack.(address)
;;

let last_popped_element t = t.stack.(t.stack_pointer)

let iter fn t =
  for i = 0 to t.stack_pointer - 1 do
    fn @@ nth t i
  done
;;
