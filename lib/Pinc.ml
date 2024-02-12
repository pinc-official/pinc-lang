module Ast = Pinc_Frontend.Ast
module Parser = Pinc_Frontend.Parser

module Interpreter = struct
  include Pinc_Backend.Pinc_Interpreter
  module Types = Pinc_Backend.Pinc_Interpreter.Types
end

module StringMap = Map.Make (String)
module Typer = Pinc_Backend.Pinc_Typer
