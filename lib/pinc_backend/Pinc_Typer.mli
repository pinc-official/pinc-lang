module Expect : sig
  val required : ('a -> 'b option) -> 'a -> 'b
  val maybe : ('a -> 'b option) -> 'a -> 'b option
  val attribute : string -> ('a -> 'b option) -> 'a StringMap.t -> 'b option
  val any_value : 'a -> 'a
  val string : Pinc_Interpreter_Types.value -> string option
  val int : Pinc_Interpreter_Types.value -> int option
  val float : Pinc_Interpreter_Types.value -> float option
  val bool : Pinc_Interpreter_Types.value -> bool option

  val array :
    (Pinc_Interpreter_Types.value -> 'a) ->
    Pinc_Interpreter_Types.value ->
    'a array option

  val list :
    (Pinc_Interpreter_Types.value -> 'a) -> Pinc_Interpreter_Types.value -> 'a list option

  val record :
    Pinc_Interpreter_Types.value -> Pinc_Interpreter_Types.value StringMap.t option

  val definition_info :
    ?typ:[< `All | `Component | `Library | `Page | `Store > `All ] ->
    Pinc_Interpreter_Types.value ->
    ([> `Component | `Library | `Page | `Store ] * string * bool) option
end
