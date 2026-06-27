  $ NO_COLOR="1" print_vm . Char
  d

  $ NO_COLOR="1" print_instructions . Char
  [CONSTANTS]
  0x00000001 (00000001) : 'c'
  0x00000002 (00000002) : 1
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Add
  0011 I_Pop
