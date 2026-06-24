  $ NO_COLOR="1" print_vm . Add
  12

  $ NO_COLOR="1" print_instructions . Add
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Add
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Sub
  2

  $ NO_COLOR="1" print_instructions . Sub
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Sub
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Div
  1.4

  $ NO_COLOR="1" print_instructions . Div
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Div
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Mul
  35

  $ NO_COLOR="1" print_instructions . Mul
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Mul
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Mod
  2

  $ NO_COLOR="1" print_instructions . Mod
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Mod
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Pow
  16807

  $ NO_COLOR="1" print_instructions . Pow
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Pow
  0011 I_Pop

  $ NO_COLOR="1" print_vm . MinusInt
  -5

  $ NO_COLOR="1" print_instructions . MinusInt
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Minus
  0006 I_Pop

  $ NO_COLOR="1" print_vm . MinusFloat
  -3.14

  $ NO_COLOR="1" print_instructions . MinusFloat
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Minus
  0006 I_Pop

  $ NO_COLOR="1" print_vm . Math
  5

  $ NO_COLOR="1" print_instructions . Math
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

  $ NO_COLOR="1" print_vm . True
  true

  $ NO_COLOR="1" print_instructions . True
  0000 I_True
  0001 I_Pop

  $ NO_COLOR="1" print_vm . False
  false

  $ NO_COLOR="1" print_instructions . False
  0000 I_False
  0001 I_Pop

  $ NO_COLOR="1" print_vm . And
  false

  $ NO_COLOR="1" print_instructions . And
  0000 I_False
  0001 I_True
  0002 I_And
  0003 I_Pop

  $ NO_COLOR="1" print_vm . Or
  true

  $ NO_COLOR="1" print_instructions . Or
  0000 I_False
  0001 I_True
  0002 I_Or
  0003 I_Pop

  $ NO_COLOR="1" print_vm . Equal
  false

  $ NO_COLOR="1" print_instructions . Equal
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . NotEqual
  true

  $ NO_COLOR="1" print_instructions . NotEqual
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Not_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Greater
  false

  $ NO_COLOR="1" print_instructions . Greater
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Greater
  0011 I_Pop

  $ NO_COLOR="1" print_vm . GreaterEqual
  false

  $ NO_COLOR="1" print_instructions . GreaterEqual
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Greater_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Less
  true

  $ NO_COLOR="1" print_instructions . Less
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Less
  0011 I_Pop

  $ NO_COLOR="1" print_vm . LessEqual
  true

  $ NO_COLOR="1" print_instructions . LessEqual
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Less_Equal
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Not
  false

  $ NO_COLOR="1" print_instructions . Not
  0000 I_True
  0001 I_Not
  0002 I_Pop

  $ NO_COLOR="1" print_vm . IfTrue
  true

  $ NO_COLOR="1" print_instructions . IfTrue
  0000 I_True
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_Null
  0013 I_Pop

  $ NO_COLOR="1" print_vm . IfTrueElse
  true

  $ NO_COLOR="1" print_instructions . IfTrueElse
  0000 I_True
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_False
  0013 I_Pop

  $ NO_COLOR="1" print_vm . IfFalse
  

  $ NO_COLOR="1" print_instructions . IfFalse
  0000 I_False
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_Null
  0013 I_Pop

  $ NO_COLOR="1" print_vm . IfFalseElse
  false

  $ NO_COLOR="1" print_instructions . IfFalseElse
  0000 I_False
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_False
  0013 I_Pop

  $ NO_COLOR="1" print_vm . Let
  1

  $ NO_COLOR="1" print_instructions . Let
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Get_Global 0x00000001 (00000001)
  0015 I_Set_Global 0x00000002 (00000002)
  0020 I_Get_Global 0x00000002 (00000002)
  0025 I_Pop

  $ NO_COLOR="1" print_instructions . UnboundIdentifier
  
  ERROR in file ./bindings.pi:8:13-16
  
     7 │ component UnboundIdentifier {
     8 │   let two = one;
       │             ^^^ 
     9 │   two
  
  Unbound identifier `one`
  [1]
