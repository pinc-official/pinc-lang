  $ NO_COLOR="1" print_vm . True
  true

  $ NO_COLOR="1" print_instructions . True
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Pop

  $ NO_COLOR="1" print_vm . False
  false

  $ NO_COLOR="1" print_instructions . False
  [INSTRUCTIONS]
  0000 I_False
  0001 I_Pop

  $ NO_COLOR="1" print_vm . And
  false

  $ NO_COLOR="1" print_instructions . And
  [INSTRUCTIONS]
  0000 I_False
  0001 I_True
  0002 I_And
  0003 I_Pop

  $ NO_COLOR="1" print_vm . Or
  true

  $ NO_COLOR="1" print_instructions . Or
  [INSTRUCTIONS]
  0000 I_False
  0001 I_True
  0002 I_Or
  0003 I_Pop

  $ NO_COLOR="1" print_vm . Equal
  false

  $ NO_COLOR="1" print_instructions . Equal
  [CONSTANTS]
  0x00000001 (00000001) : 3
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . NotEqual
  true

  $ NO_COLOR="1" print_instructions . NotEqual
  [CONSTANTS]
  0x00000001 (00000001) : 3
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Not_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Greater
  false

  $ NO_COLOR="1" print_instructions . Greater
  [CONSTANTS]
  0x00000001 (00000001) : 3
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Greater
  0011 I_Pop

  $ NO_COLOR="1" print_vm . GreaterEqual
  false

  $ NO_COLOR="1" print_instructions . GreaterEqual
  [CONSTANTS]
  0x00000001 (00000001) : 3
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Greater_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Less
  true

  $ NO_COLOR="1" print_instructions . Less
  [CONSTANTS]
  0x00000001 (00000001) : 3
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Less
  0011 I_Pop

  $ NO_COLOR="1" print_vm . LessEqual
  true

  $ NO_COLOR="1" print_instructions . LessEqual
  [CONSTANTS]
  0x00000001 (00000001) : 3
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Less_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Not
  false

  $ NO_COLOR="1" print_instructions . Not
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Not
  0002 I_Pop
