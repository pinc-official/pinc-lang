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

  $ NO_COLOR="1" print_vm . IfTrue
  true

  $ NO_COLOR="1" print_instructions . IfTrue
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_Null
  0013 I_Pop

  $ NO_COLOR="1" print_vm . IfTrueElse
  true

  $ NO_COLOR="1" print_instructions . IfTrueElse
  [INSTRUCTIONS]
  0000 I_True
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_False
  0013 I_Pop

  $ NO_COLOR="1" print_vm . IfFalse
  

  $ NO_COLOR="1" print_instructions . IfFalse
  [INSTRUCTIONS]
  0000 I_False
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_Null
  0013 I_Pop

  $ NO_COLOR="1" print_vm . IfFalseElse
  false

  $ NO_COLOR="1" print_instructions . IfFalseElse
  [INSTRUCTIONS]
  0000 I_False
  0001 I_Jump_If_False 0x0000000C (00000012)
  0006 I_True
  0007 I_Jump 0x0000000D (00000013)
  0012 I_False
  0013 I_Pop

  $ NO_COLOR="1" print_vm . Let
  1

  $ NO_COLOR="1" print_instructions . Let
  [CONSTANTS]
  0x00000001 (00000001) : 1
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Get_Global 0x00000001 (00000001)
  0015 I_Set_Global 0x00000002 (00000002)
  0020 I_Get_Global 0x00000002 (00000002)
  0025 I_Pop

  $ NO_COLOR="1" print_vm . Shadowing
  2

  $ NO_COLOR="1" print_instructions . Shadowing
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 1
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Get_Global 0x00000001 (00000001)
  0015 I_Constant 0x00000002 (00000002)
  0020 I_Add
  0021 I_Set_Global 0x00000002 (00000002)
  0026 I_Get_Global 0x00000002 (00000002)
  0031 I_Pop

  $ NO_COLOR="1" print_instructions . UnboundIdentifier
  
  ERROR in file ./bindings.pi:8:13-16
  
     7 │ component UnboundIdentifier {
     8 │   let two = one;
       │             ^^^ 
     9 │   two
  
  Unbound identifier `one`
  [1]

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

  $ NO_COLOR="1" print_vm . EmptyArray
  

  $ NO_COLOR="1" print_instructions . EmptyArray
  [INSTRUCTIONS]
  0000 I_Array 0
  0005 I_Pop

  $ NO_COLOR="1" print_vm . Array
  1 2 3 4 5

  $ NO_COLOR="1" print_instructions . Array
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 2
  0x00000003 (00000003) : 3
  0x00000004 (00000004) : 4
  0x00000005 (00000005) : 5
  
  [INSTRUCTIONS]
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
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 2
  0x00000003 (00000003) : 3
  0x00000004 (00000004) : 4
  0x00000005 (00000005) : 5
  0x00000006 (00000006) : 6
  0x00000007 (00000007) : 7
  0x00000008 (00000008) : 8
  
  [INSTRUCTIONS]
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

  $ NO_COLOR="1" print_vm . ArrayAccess
  -1

  $ NO_COLOR="1" print_instructions . ArrayAccess
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 2
  0x00000003 (00000003) : 3
  0x00000004 (00000004) : 4
  0x00000005 (00000005) : 5
  0x00000006 (00000006) : 6
  0x00000007 (00000007) : 7
  0x00000008 (00000008) : 8
  0x00000009 (00000009) : 3
  0x0000000A (00000010) : 2
  
  [INSTRUCTIONS]
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
  0049 I_Constant 0x00000009 (00000009)
  0054 I_Constant 0x0000000A (00000010)
  0059 I_Sub
  0060 I_Index
  0061 I_Pop

  $ NO_COLOR="1" print_vm . ArrayRange
  1 2 3 4 5 6 7 8 9

  $ NO_COLOR="1" print_instructions . ArrayRange
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 10
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Range
  0011 I_Pop

  $ NO_COLOR="1" print_vm . ArrayRangeInclusive
  1 2 3 4 5 6 7 8 9 10

  $ NO_COLOR="1" print_instructions . ArrayRangeInclusive
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 10
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Range_Inclusive
  0011 I_Pop

  $ NO_COLOR="1" print_vm . Record
  1
  foo
  true
  3.1415

  $ NO_COLOR="1" print_instructions . Record
  [CONSTANTS]
  0x00000001 (00000001) : "a"
  0x00000002 (00000002) : "b"
  0x00000003 (00000003) : "c"
  0x00000004 (00000004) : "d"
  0x00000005 (00000005) : 1
  0x00000006 (00000006) : "foo"
  0x00000007 (00000007) : 3.141500
  
  [INSTRUCTIONS]
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
  [INSTRUCTIONS]
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
  [CONSTANTS]
  0x00000001 (00000001) : "a"
  0x00000002 (00000002) : "b"
  0x00000003 (00000003) : "c"
  0x00000004 (00000004) : "d"
  0x00000005 (00000005) : 1
  0x00000006 (00000006) : "foo"
  0x00000007 (00000007) : "e"
  0x00000008 (00000008) : "f"
  0x00000009 (00000009) : "g"
  0x0000000A (00000010) : "123"
  0x0000000B (00000011) : 321
  0x0000000C (00000012) : 1
  0x0000000D (00000013) : 2
  0x0000000E (00000014) : 3
  0x0000000F (00000015) : 4
  0x00000010 (00000016) : 5
  0x00000011 (00000017) : 6
  0x00000012 (00000018) : 7
  0x00000013 (00000019) : 8
  
  [INSTRUCTIONS]
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

  $ NO_COLOR="1" print_vm . RecordNested
  1
  foo
  123
  321
  1 2 3 4 5 6 7 8
  true

  $ NO_COLOR="1" print_instructions . RecordNested
  [CONSTANTS]
  0x00000001 (00000001) : "a"
  0x00000002 (00000002) : "b"
  0x00000003 (00000003) : "c"
  0x00000004 (00000004) : "d"
  0x00000005 (00000005) : 1
  0x00000006 (00000006) : "foo"
  0x00000007 (00000007) : "e"
  0x00000008 (00000008) : "f"
  0x00000009 (00000009) : "g"
  0x0000000A (00000010) : "123"
  0x0000000B (00000011) : 321
  0x0000000C (00000012) : 1
  0x0000000D (00000013) : 2
  0x0000000E (00000014) : 3
  0x0000000F (00000015) : 4
  0x00000010 (00000016) : 5
  0x00000011 (00000017) : 6
  0x00000012 (00000018) : 7
  0x00000013 (00000019) : 8
  
  [INSTRUCTIONS]
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

  $ NO_COLOR="1" print_vm . RecordAccessDot
  123

  $ NO_COLOR="1" print_instructions . RecordAccessDot
  [CONSTANTS]
  0x00000001 (00000001) : "a"
  0x00000002 (00000002) : "c"
  0x00000003 (00000003) : 1
  0x00000004 (00000004) : "e"
  0x00000005 (00000005) : "123"
  0x00000006 (00000006) : "c"
  0x00000007 (00000007) : "e"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Constant 0x00000003 (00000003)
  0015 I_Constant 0x00000004 (00000004)
  0020 I_Constant 0x00000005 (00000005)
  0025 I_Record 1
  0030 I_Record 2
  0035 I_Set_Global 0x00000001 (00000001)
  0040 I_Get_Global 0x00000001 (00000001)
  0045 I_Constant 0x00000006 (00000006)
  0050 I_Dot_Index
  0051 I_Constant 0x00000007 (00000007)
  0056 I_Dot_Index
  0057 I_Pop

  $ NO_COLOR="1" print_vm . RecordAccessBracket
  123

  $ NO_COLOR="1" print_instructions . RecordAccessBracket
  [CONSTANTS]
  0x00000001 (00000001) : "a"
  0x00000002 (00000002) : "c"
  0x00000003 (00000003) : 1
  0x00000004 (00000004) : "e"
  0x00000005 (00000005) : "123"
  0x00000006 (00000006) : "c"
  0x00000007 (00000007) : "e"
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Constant 0x00000002 (00000002)
  0010 I_Constant 0x00000003 (00000003)
  0015 I_Constant 0x00000004 (00000004)
  0020 I_Constant 0x00000005 (00000005)
  0025 I_Record 1
  0030 I_Record 2
  0035 I_Set_Global 0x00000001 (00000001)
  0040 I_Get_Global 0x00000001 (00000001)
  0045 I_Constant 0x00000006 (00000006)
  0050 I_Index
  0051 I_Constant 0x00000007 (00000007)
  0056 I_Index
  0057 I_Pop

  $ NO_COLOR="1" print_vm . FunctionEmpty
  

  $ NO_COLOR="1" print_instructions . FunctionEmpty
  [CONSTANTS]
  0x00000001 (00000001) : <FUNCTION> [
    0000 I_Null
    0001 I_Return
  ]
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000001 (00000001)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Get_Global 0x00000001 (00000001)
  0015 I_Call
  0016 I_Pop

  $ NO_COLOR="1" print_vm . Function
  3

  $ NO_COLOR="1" print_instructions . Function
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 2
  0x00000003 (00000003) : <FUNCTION> [
    0000 I_Constant 0x00000001 (00000001)
    0005 I_Set_Global 0x00000001 (00000001)
    0010 I_Constant 0x00000002 (00000002)
    0015 I_Set_Global 0x00000002 (00000002)
    0020 I_Get_Global 0x00000001 (00000001)
    0025 I_Get_Global 0x00000002 (00000002)
    0030 I_Add
    0031 I_Return
  ]
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000003 (00000003)
  0005 I_Set_Global 0x00000003 (00000003)
  0010 I_Get_Global 0x00000003 (00000003)
  0015 I_Call
  0016 I_Pop

  $ NO_COLOR="1" print_vm . FunctionCurried
  1

  $ NO_COLOR="1" print_instructions . FunctionCurried
  [CONSTANTS]
  0x00000001 (00000001) : 1
  0x00000002 (00000002) : 2
  0x00000003 (00000003) : <FUNCTION> [
    0000 I_True
    0001 I_Jump_If_False 0x00000010 (00000016)
    0006 I_Constant 0x00000001 (00000001)
    0011 I_Jump 0x00000015 (00000021)
    0016 I_Constant 0x00000002 (00000002)
    0021 I_Return
  ]
  0x00000004 (00000004) : <FUNCTION> [
    0000 I_Get_Global 0x00000001 (00000001)
    0005 I_Return
  ]
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000003 (00000003)
  0005 I_Set_Global 0x00000001 (00000001)
  0010 I_Constant 0x00000004 (00000004)
  0015 I_Set_Global 0x00000002 (00000002)
  0020 I_Get_Global 0x00000002 (00000002)
  0025 I_Call
  0026 I_Call
  0027 I_Pop
