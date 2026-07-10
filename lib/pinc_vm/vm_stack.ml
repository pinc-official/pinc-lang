exception Pinc_stack_overflow

type tag =
  | Tag_Null
  | Tag_Int
  | Tag_Bool
  | Tag_Char
  | Tag_Float
  | Tag_Obj

type t = {
  stack_size : int;
  mutable stack_pointer : int;
  (* Tracks the type of every slot *)
  tag_stack : tag array;
  (* Holds raw unboxed ints, bools, chars *)
  int_stack : int array;
  (* Holds raw unboxed 64-bit floats *)
  float_stack : float array;
  (* Holds remaining heap objects (Strings, Closures, Arrays) *)
  obj_stack : Pinc_Bytecode.Value.t array;
}

let make ~size =
  {
    stack_size = size;
    stack_pointer = 0;
    tag_stack = Array.make size Tag_Null;
    int_stack = Array.make size 0;
    float_stack = Array.make size 0.0;
    obj_stack = Array.make size Pinc_Bytecode.Value.Null;
  }
;;

let[@inline] set_pointer t n =
  if n > t.stack_size then
    raise_notrace Pinc_stack_overflow
  else
    t.stack_pointer <- n
;;

let[@inline] peek_tag t offset =
  Array.unsafe_get t.tag_stack (t.stack_pointer - 1 - offset)
;;

let[@inline] set_tag t offset tag =
  Array.unsafe_set t.tag_stack (t.stack_pointer - 1 - offset) tag
;;

let[@inline] is_int t offset = peek_tag t offset = Tag_Int

let[@inline] push_int t v =
  let sp = t.stack_pointer in
  if sp >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    Array.unsafe_set t.tag_stack sp Tag_Int;
    Array.unsafe_set t.int_stack sp v;
    t.stack_pointer <- sp + 1)
;;

let[@inline] pop_int t =
  let sp = t.stack_pointer - 1 in
  t.stack_pointer <- sp;
  Array.unsafe_get t.int_stack sp
;;

let[@inline] is_float t offset = peek_tag t offset = Tag_Float

let[@inline] push_float t v =
  if t.stack_pointer >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    Array.unsafe_set t.tag_stack t.stack_pointer Tag_Float;
    Array.unsafe_set t.float_stack t.stack_pointer v;
    t.stack_pointer <- succ t.stack_pointer)
;;

let[@inline] pop_float t =
  let sp = t.stack_pointer - 1 in
  t.stack_pointer <- sp;
  Array.unsafe_get t.float_stack sp
;;

let[@inline] is_char t offset = peek_tag t offset = Tag_Char

let[@inline] push_char t v =
  if t.stack_pointer >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    Array.unsafe_set t.tag_stack t.stack_pointer Tag_Char;
    Array.unsafe_set t.int_stack t.stack_pointer v;
    t.stack_pointer <- succ t.stack_pointer)
;;

let[@inline] pop_char t =
  let sp = t.stack_pointer - 1 in
  t.stack_pointer <- sp;
  let c = Array.unsafe_get t.int_stack sp in
  c
;;

let[@inline] is_bool t offset = peek_tag t offset = Tag_Bool

let[@inline] push_bool t v =
  let sp = t.stack_pointer in
  if sp >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    Array.unsafe_set t.tag_stack sp Tag_Bool;
    Array.unsafe_set t.int_stack sp (Bool.to_int v);
    t.stack_pointer <- sp + 1)
;;

let[@inline] pop_bool t =
  let sp = t.stack_pointer - 1 in
  t.stack_pointer <- sp;
  Array.unsafe_get t.int_stack sp = 1
;;

let[@inline] is_null t offset = peek_tag t offset = Tag_Null

let[@inline] push_null t =
  let sp = t.stack_pointer in
  if sp >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    Array.unsafe_set t.tag_stack sp Tag_Null;
    t.stack_pointer <- sp + 1)
;;

let[@inline] copy_to_top t from =
  let to_ = t.stack_pointer in
  if to_ >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    let tag = Array.unsafe_get t.tag_stack from in
    Array.unsafe_set t.tag_stack to_ tag;
    match tag with
    | Tag_Int -> Array.unsafe_set t.int_stack to_ (Array.unsafe_get t.int_stack from)
    | Tag_Float ->
        Array.unsafe_set t.float_stack to_ (Array.unsafe_get t.float_stack from)
    | Tag_Bool -> Array.unsafe_set t.int_stack to_ (Array.unsafe_get t.int_stack from)
    | Tag_Char -> Array.unsafe_set t.int_stack to_ (Array.unsafe_get t.int_stack from)
    | Tag_Obj -> Array.unsafe_set t.obj_stack to_ (Array.unsafe_get t.obj_stack from)
    | Tag_Null -> ());
  t.stack_pointer <- to_ + 1
;;

let[@inline] move_from_top t to_ =
  let from = t.stack_pointer - 1 in
  if to_ >= t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    let tag = Array.unsafe_get t.tag_stack from in
    Array.unsafe_set t.tag_stack to_ tag;
    match tag with
    | Tag_Int -> Array.unsafe_set t.int_stack to_ (Array.unsafe_get t.int_stack from)
    | Tag_Float ->
        Array.unsafe_set t.float_stack to_ (Array.unsafe_get t.float_stack from)
    | Tag_Bool -> Array.unsafe_set t.int_stack to_ (Array.unsafe_get t.int_stack from)
    | Tag_Char -> Array.unsafe_set t.int_stack to_ (Array.unsafe_get t.int_stack from)
    | Tag_Obj -> Array.unsafe_set t.obj_stack to_ (Array.unsafe_get t.obj_stack from)
    | Tag_Null -> ());
  t.stack_pointer <- from
;;

let[@inline] set t address value =
  if address >= t.stack_size then
    raise_notrace Pinc_stack_overflow;
  match value with
  | Pinc_Bytecode.Value.Null -> Array.unsafe_set t.tag_stack address Tag_Null
  | Pinc_Bytecode.Value.Int i ->
      Array.unsafe_set t.tag_stack address Tag_Int;
      Array.unsafe_set t.int_stack address i
  | Pinc_Bytecode.Value.Float f ->
      Array.unsafe_set t.tag_stack address Tag_Float;
      Array.unsafe_set t.float_stack address f
  | Pinc_Bytecode.Value.Bool true ->
      Array.unsafe_set t.tag_stack address Tag_Bool;
      Array.unsafe_set t.int_stack address 1
  | Pinc_Bytecode.Value.Bool false ->
      Array.unsafe_set t.tag_stack address Tag_Bool;
      Array.unsafe_set t.int_stack address 0
  | Pinc_Bytecode.Value.Char c ->
      Array.unsafe_set t.tag_stack address Tag_Char;
      Array.unsafe_set t.int_stack address (Uchar.to_int c)
  | obj ->
      Array.unsafe_set t.tag_stack address Tag_Obj;
      Array.unsafe_set t.obj_stack address obj
;;

let[@inline] get t address =
  if address > t.stack_size then
    raise_notrace Pinc_stack_overflow
  else (
    match
      Array.unsafe_get t.tag_stack address
    with
    | Tag_Int ->
        let i = Array.unsafe_get t.int_stack address in
        Pinc_Bytecode.Value.Int i
    | Tag_Float ->
        let f = Array.unsafe_get t.float_stack address in
        Pinc_Bytecode.Value.Float f
    | Tag_Bool ->
        let b = Array.unsafe_get t.int_stack address = 1 in
        if b then
          Pinc_Bytecode.Value.constant_true
        else
          Pinc_Bytecode.Value.constant_false
    | Tag_Char ->
        let c = Uchar.unsafe_of_int (Array.unsafe_get t.int_stack address) in
        Pinc_Bytecode.Value.Char c
    | Tag_Obj -> Array.unsafe_get t.obj_stack address
    | Tag_Null -> Pinc_Bytecode.Value.Null)
;;

let[@inline] push_value t value =
  let () = set t t.stack_pointer value in
  t.stack_pointer <- t.stack_pointer + 1
;;

let[@inline] pop_value t =
  t.stack_pointer <- t.stack_pointer - 1;
  get t t.stack_pointer
;;

let[@inline] nth t n =
  let target = t.stack_pointer - 1 - n in
  if target < 0 then
    Pinc_Bytecode.Value.Null
  else
    get t target
;;

let[@inline] top t = nth t 0
let[@inline] drop t = t.stack_pointer <- t.stack_pointer - 1

let[@inline] pop_n t n =
  t.stack_pointer <- t.stack_pointer - n;
  List.init n (fun index -> get t (t.stack_pointer + index))
;;

let[@inline] last_popped_element t = get t t.stack_pointer

let iter fn t =
  for i = 0 to t.stack_pointer - 1 do
    fn @@ nth t i
  done
;;
