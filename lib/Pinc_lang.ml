module Diagnostics = Pinc_Diagnostics
module Source = Pinc_Source
module Ast = Pinc_Parser.Ast
module Parser = Pinc_Parser
module Typer = Pinc_Typer

module Interpreter = struct
  include Pinc_Backend.Interpreter
  module Types = Pinc_Backend.Interpreter.Types
end

module StringMap = Pinc_Core.StringMap
module Helpers = Pinc_Backend.Helpers
