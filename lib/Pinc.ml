module Ast = struct
  include Pinc_Ast
end

module Token = struct
  include Pinc_Token
end

module Lexer = struct
  include Pinc_Lexer
end

module Parser = struct
  include Pinc_Parser
end

module Interpreter = struct
  include Pinc_Interpreter

  module Types = struct
    include Pinc_Interpreter_Types
  end
end

module StringMap = struct
  include StringMap
end

module Typer = struct
  include Pinc_Typer
end
