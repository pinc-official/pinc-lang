  $ NO_COLOR="1" print_instructions . IfTrue
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Jump_If_False 0x00000004 (00000004)
  0006 I_True
  0007 I_Jump 0x00000005 (00000005)
  0012 I_Null


  $ NO_COLOR="1" print_instructions . IfTrueElse
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Jump_If_False 0x00000004 (00000004)
  0006 I_True
  0007 I_Jump 0x00000005 (00000005)
  0012 I_False


  $ NO_COLOR="1" print_instructions . IfFalse
  [INSTRUCTIONS]
  0000 I_False
  0001 I_Jump_If_False 0x00000004 (00000004)
  0006 I_True
  0007 I_Jump 0x00000005 (00000005)
  0012 I_Null


  $ NO_COLOR="1" print_instructions . IfFalseElse
  [INSTRUCTIONS]
  0000 I_False
  0001 I_Jump_If_False 0x00000004 (00000004)
  0006 I_True
  0007 I_Jump 0x00000005 (00000005)
  0012 I_False
