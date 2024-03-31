module Source = Pinc_Source
module Ast = Pinc_Parser.Ast
module Parser = Pinc_Parser

module Interpreter = struct
  include Pinc_Backend.Interpreter
  module Types = Pinc_Backend.Interpreter.Types
end

module StringMap = Pinc_Backend.StringMap
module Value = Pinc_Backend.Value
module Typer = Pinc_Backend.Typer
