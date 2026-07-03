  $ NO_COLOR="1" print_instructions . String
  [CONSTANTS]
  0x00000000 (00000000) : "Hello!"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Pop

  $ NO_COLOR="1" print_instructions . StringConcat
  [CONSTANTS]
  0x00000000 (00000000) : "Hello "
  0x00000001 (00000001) : "World"
  0x00000002 (00000002) : "!"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Concat
  0011 I_Constant 0x00000002 (00000002)
  0016 I_Concat
  0017 I_Pop

  $ NO_COLOR="1" print_instructions . StringInterpolation
  [CONSTANTS]
  0x00000000 (00000000) : "World"
  0x00000001 (00000001) : "Hello "
  0x00000002 (00000002) : "!"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_Constant 0x00000001 (00000001)
  0017 I_Get_Global 0x00000000 (00000000)
  0022 I_Concat
  0023 I_Constant 0x00000002 (00000002)
  0028 I_Concat
  0029 I_Pop

  $ NO_COLOR="1" print_instructions . EmptyString
  [CONSTANTS]
  0x00000000 (00000000) : ""
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Pop
