val make
  :  eval:
       (Pinc_Interpreter_Generic.State.t
        -> Pinc_Interpreter_Generic.Tag.info
        -> (Pinc_Interpreter_Generic.Value.t, string) result)
  -> validate:
       (Pinc_Interpreter_Generic.Tag.info
        -> Pinc_Interpreter_Generic.Value.t
        -> (Pinc_Interpreter_Generic.Value.t, string) result)
  -> transform:
       (Pinc_Interpreter_Generic.Tag.info
        -> Pinc_Interpreter_Generic.Value.t
        -> Pinc_Interpreter_Generic.Value.t)
  -> Pinc_Interpreter_Generic.Tag.t

module Default : sig
  val string : Pinc_Interpreter_Generic.Tag.t
  val int : Pinc_Interpreter_Generic.Tag.t
  val float : Pinc_Interpreter_Generic.Tag.t
  val boolean : Pinc_Interpreter_Generic.Tag.t
  val array : Pinc_Interpreter_Generic.Tag.t
  val record : Pinc_Interpreter_Generic.Tag.t
  val slot : Pinc_Interpreter_Generic.Tag.t
  val set_context : Pinc_Interpreter_Generic.Tag.t
  val get_context : Pinc_Interpreter_Generic.Tag.t
end
