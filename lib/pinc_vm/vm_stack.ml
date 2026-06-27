exception Pinc_stack_overflow

type 'a t = {
  default_value : 'a;
  stack_size : int;
  mutable stack_pointer : int;
  mutable stack : 'a Array.t;
}

let make ~size ~default_value =
  {
    default_value;
    stack_size = size;
    stack_pointer = 0;
    stack = Array.make size default_value;
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

let top t =
  match t.stack_pointer with
  | 0 -> t.default_value
  | n -> t.stack.(n - 1)
;;

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
