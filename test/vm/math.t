  $ NO_COLOR="1" print_vm . Add
  12

  $ NO_COLOR="1" print_instructions . Add
  [CONSTANTS]
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Add
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Sub
  2

  $ NO_COLOR="1" print_instructions . Sub
  [CONSTANTS]
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Sub
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Div
  1.4

  $ NO_COLOR="1" print_instructions . Div
  [CONSTANTS]
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Div
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Mul
  35

  $ NO_COLOR="1" print_instructions . Mul
  [CONSTANTS]
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Mul
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Mod
  2

  $ NO_COLOR="1" print_instructions . Mod
  [CONSTANTS]
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Mod
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Pow
  16807

  $ NO_COLOR="1" print_instructions . Pow
  [CONSTANTS]
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Pow
  0011 I_Pop

  $ NO_COLOR="1" print_vm . MinusInt
  -5

  $ NO_COLOR="1" print_instructions . MinusInt
  [CONSTANTS]
  0x00000001 (00000001) : 5
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Minus
  0006 I_Pop

  $ NO_COLOR="1" print_vm . MinusFloat
  -3.14

  $ NO_COLOR="1" print_instructions . MinusFloat
  [CONSTANTS]
  0x00000001 (00000001) : 3.140000
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Minus
  0006 I_Pop

  $ NO_COLOR="1" print_vm . Math
  5

  $ NO_COLOR="1" print_instructions . Math
  [CONSTANTS]
  0x00000001 (00000001) : 5
  0x00000002 (00000002) : 3
  0x00000003 (00000003) : 2
  0x00000004 (00000004) : 6
  0x00000005 (00000005) : 4
  0x00000006 (00000006) : 8
  0x00000007 (00000007) : 10
  0x00000008 (00000008) : 10
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Mul
  0011 I_Constant 0x00000003 (00000003)
  0016 I_Constant 0x00000004 (00000004)
  0021 I_Constant 0x00000005 (00000005)
  0026 I_Constant 0x00000006 (00000006)
  0031 I_Mul
  0032 I_Add
  0033 I_Constant 0x00000007 (00000007)
  0038 I_Pow
  0039 I_Div
  0040 I_Add
  0041 I_Constant 0x00000008 (00000008)
  0046 I_Minus
  0047 I_Add
  0048 I_Pop
