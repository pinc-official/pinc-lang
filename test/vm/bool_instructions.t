  $ NO_COLOR="1" print_instructions . True
  [INSTRUCTIONS]
  0000 I_True


  $ NO_COLOR="1" print_instructions . False
  [INSTRUCTIONS]
  0000 I_False


  $ NO_COLOR="1" print_instructions . And
  [INSTRUCTIONS]
  0000 I_False
  0001 I_True
  0002 I_And


  $ NO_COLOR="1" print_instructions . Or
  [INSTRUCTIONS]
  0000 I_False
  0001 I_True
  0002 I_Or


  $ NO_COLOR="1" print_instructions . Equal
  [CONSTANTS]
  0x00000000 (00000000) : 3
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Equal


  $ NO_COLOR="1" print_instructions . NotEqual
  [CONSTANTS]
  0x00000000 (00000000) : 3
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Not_Equal


  $ NO_COLOR="1" print_instructions . Greater
  [CONSTANTS]
  0x00000000 (00000000) : 3
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Greater


  $ NO_COLOR="1" print_instructions . GreaterEqual
  [CONSTANTS]
  0x00000000 (00000000) : 3
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Greater_Equal


  $ NO_COLOR="1" print_instructions . Less
  [CONSTANTS]
  0x00000000 (00000000) : 3
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Less


  $ NO_COLOR="1" print_instructions . LessEqual
  [CONSTANTS]
  0x00000000 (00000000) : 3
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Constant 0x00000001 (00000001)
  0010 I_Less_Equal


  $ NO_COLOR="1" print_instructions . Not
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Not
