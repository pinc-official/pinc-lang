module Source = Pinc_Core.Source
module Ast = Pinc_Frontend.Ast
module Parser = Pinc_Frontend.Parser

module Interpreter = struct
  include Pinc_Backend.Interpreter
  module Types = Pinc_Backend.Interpreter.Types
end

module StringMap = Pinc_Backend.StringMap
module Value = Pinc_Backend.Value
module Typer = Pinc_Backend.Typer
