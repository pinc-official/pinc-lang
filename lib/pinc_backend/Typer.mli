module Expect : sig
  val required : ('a -> 'b option) -> 'a -> 'b
  val maybe : ('a -> 'b option) -> 'a -> 'b option
  val attribute : string -> ('a -> 'b option) -> 'a StringMap.t -> 'b option
  val any_value : 'a -> 'a
  val string : Interpreter.Types.value -> string option
  val int : Interpreter.Types.value -> int option
  val float : Interpreter.Types.value -> float option
  val bool : Interpreter.Types.value -> bool option

  val array :
    (Interpreter.Types.value -> 'a) -> Interpreter.Types.value -> 'a array option

  val list : (Interpreter.Types.value -> 'a) -> Interpreter.Types.value -> 'a list option
  val record : Interpreter.Types.value -> Interpreter.Types.value StringMap.t option

  val definition_info :
    ?typ:[< `All | `Component | `Library | `Page | `Store > `All ] ->
    Interpreter.Types.value ->
    ([> `Component | `Library | `Page | `Store ] * string * bool) option
end
