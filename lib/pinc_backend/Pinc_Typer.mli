module Expect : sig
  val required : ('a -> 'b option) -> 'a -> 'b
  val maybe : ('a -> 'b option) -> 'a -> 'b option
  val attribute : string -> ('a -> 'b option) -> 'a StringMap.t -> 'b option
  val any_value : 'a -> 'a
  val string : Pinc_Interpreter.Types.value -> string option
  val int : Pinc_Interpreter.Types.value -> int option
  val float : Pinc_Interpreter.Types.value -> float option
  val bool : Pinc_Interpreter.Types.value -> bool option

  val array :
    (Pinc_Interpreter.Types.value -> 'a) ->
    Pinc_Interpreter.Types.value ->
    'a array option

  val list :
    (Pinc_Interpreter.Types.value -> 'a) -> Pinc_Interpreter.Types.value -> 'a list option

  val record :
    Pinc_Interpreter.Types.value -> Pinc_Interpreter.Types.value StringMap.t option

  val definition_info :
    ?typ:[< `All | `Component | `Library | `Page | `Store > `All ] ->
    Pinc_Interpreter.Types.value ->
    ([> `Component | `Library | `Page | `Store ] * string * bool) option
end
