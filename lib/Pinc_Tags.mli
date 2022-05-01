open Pinc_Interpreter_Types

val make :
     eval:(self:tag_handler -> state -> tag_info -> (value, string) result)
  -> validate:(tag_info -> value -> (value, string) result)
  -> transform:(tag_info -> value -> value)
  -> get_value:(state -> string -> value option)
  -> tag_handler

module Default : sig
  val string : tag_handler

  val int : tag_handler

  val float : tag_handler

  val boolean : tag_handler

  val array : tag_handler

  val record : tag_handler

  val slot : tag_handler

  val set_context : tag_handler

  val get_context : tag_handler

  val create_portal : tag_handler

  val push_portal : tag_handler
end
