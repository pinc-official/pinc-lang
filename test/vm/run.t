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

  $ NO_COLOR="1" print_vm . String
  Hello!

  $ NO_COLOR="1" print_instructions . String
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Pop

  $ NO_COLOR="1" print_vm . StringConcat
  Hello World!

  $ NO_COLOR="1" print_instructions . StringConcat
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Concat
  0011 I_Constant 0x00000003 (00000003)
  0016 I_Concat
  0017 I_Pop

  $ NO_COLOR="1" print_vm . StringInterpolation
  Hello World!

  $ NO_COLOR="1" print_instructions . StringInterpolation
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Constant 0x00000002 (00000002)
  0015 I_Get_Global 0x00000001 (00000001)
  0020 I_Concat
  0021 I_Constant 0x00000003 (00000003)
  0026 I_Concat
  0027 I_Pop

  $ NO_COLOR="1" print_vm . EmptyArray
  

  $ NO_COLOR="1" print_instructions . EmptyArray
  0000 I_Array 0
  0005 I_Pop

  $ NO_COLOR="1" print_vm . Array
  1 2 3 4 5

  $ NO_COLOR="1" print_instructions . Array
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Constant 0x00000003 (00000003)
  0015 I_Constant 0x00000004 (00000004)
  0020 I_Constant 0x00000005 (00000005)
  0025 I_Array 5
  0030 I_Pop

  $ NO_COLOR="1" print_vm . ExpressionArray
  3 -1 30 0.875

  $ NO_COLOR="1" print_instructions . ExpressionArray
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Add
  0011 I_Constant 0x00000003 (00000003)
  0016 I_Constant 0x00000004 (00000004)
  0021 I_Sub
  0022 I_Constant 0x00000005 (00000005)
  0027 I_Constant 0x00000006 (00000006)
  0032 I_Mul
  0033 I_Constant 0x00000007 (00000007)
  0038 I_Constant 0x00000008 (00000008)
  0043 I_Div
  0044 I_Array 4
  0049 I_Pop

  $ NO_COLOR="1" print_vm . Record
  1
  foo
  true
  3.1415

  $ NO_COLOR="1" print_instructions . Record
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Constant 0x00000003 (00000003)
  0015 I_Constant 0x00000004 (00000004)
  0020 I_Constant 0x00000005 (00000005)
  0025 I_Constant 0x00000006 (00000006)
  0030 I_True
  0031 I_Constant 0x00000007 (00000007)
  0036 I_Record 4
  0041 I_Pop

  $ NO_COLOR="1" print_vm . RecordEmpty
  

  $ NO_COLOR="1" print_instructions . RecordEmpty
  0000 I_Record 0
  0005 I_Pop

  $ NO_COLOR="1" print_vm . RecordNested
  1
  foo
  123
  321
  1 2 3 4 5 6 7 8
  true

  $ NO_COLOR="1" print_instructions . RecordNested
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Constant 0x00000003 (00000003)
  0015 I_Constant 0x00000004 (00000004)
  0020 I_Constant 0x00000005 (00000005)
  0025 I_Constant 0x00000006 (00000006)
  0030 I_Constant 0x00000007 (00000007)
  0035 I_Constant 0x00000008 (00000008)
  0040 I_Constant 0x00000009 (00000009)
  0045 I_Constant 0x0000000A (00000010)
  0050 I_Constant 0x0000000B (00000011)
  0055 I_Constant 0x0000000C (00000012)
  0060 I_Constant 0x0000000D (00000013)
  0065 I_Constant 0x0000000E (00000014)
  0070 I_Constant 0x0000000F (00000015)
  0075 I_Constant 0x00000010 (00000016)
  0080 I_Constant 0x00000011 (00000017)
  0085 I_Constant 0x00000012 (00000018)
  0090 I_Constant 0x00000013 (00000019)
  0095 I_Array 8
  0100 I_Record 3
  0105 I_True
  0106 I_Record 4
  0111 I_Pop
