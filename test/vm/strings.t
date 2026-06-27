  $ NO_COLOR="1" print_vm . String
  Hello!

  $ NO_COLOR="1" print_instructions . String
  [CONSTANTS]
  0x00000001 (00000001) : "Hello!"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Pop

  $ NO_COLOR="1" print_vm . StringConcat
  Hello World!

  $ NO_COLOR="1" print_instructions . StringConcat
  [CONSTANTS]
  0x00000001 (00000001) : "Hello "
  0x00000002 (00000002) : "World"
  0x00000003 (00000003) : "!"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Concat
  0011 I_Constant 0x00000003 (00000003)
  0016 I_Concat
  0017 I_Pop

  $ NO_COLOR="1" print_vm . StringInterpolation
  Hello World!

  $ NO_COLOR="1" print_instructions . StringInterpolation
  [CONSTANTS]
  0x00000001 (00000001) : "World"
  0x00000002 (00000002) : "Hello "
  0x00000003 (00000003) : "!"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Constant 0x00000002 (00000002)
  0015 I_Get_Global 0x00000001 (00000001)
  0020 I_Concat
  0021 I_Constant 0x00000003 (00000003)
  0026 I_Concat
  0027 I_Pop
