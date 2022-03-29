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
end
