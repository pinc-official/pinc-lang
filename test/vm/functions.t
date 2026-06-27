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
