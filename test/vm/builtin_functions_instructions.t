  $ NO_COLOR="1" print_instructions . BuiltinFunctionWrongParameters
  
  ERROR in file ./builtin_functions.pi:2:16-50
  
     1 │ component BuiltinFunctionWrongParameters {
     2 │   let length = fn (a, b) -> %%pinc_array_length%%;
       │                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
     3 │ 
  
  External function %%pinc_array_length%% expected 1 parameters, but got 2
  [1]

  $ NO_COLOR="1" print_instructions . BuiltinFunctionArrayLength
  [CONSTANTS]
  0x00000000 (00000000) : 1
  0x00000001 (00000001) : 2
  0x00000002 (00000002) : 3
  0x00000003 (00000003) : 4
  
  [INSTRUCTIONS]
  0000 I_Get_Builtin 0x00000000 (00000000)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_Get_Global 0x00000000 (00000000)
  0017 I_Constant 0x00000000 (00000000)
  0022 I_Constant 0x00000001 (00000001)
  0027 I_Constant 0x00000002 (00000002)
  0032 I_Constant 0x00000003 (00000003)
  0037 I_Array 4
  0042 I_Call 1
  0047 I_Pop

  $ NO_COLOR="1" print_instructions . BuiltinFunctionStringLength
  [CONSTANTS]
  0x00000000 (00000000) : "Hello, World!"
  
  [INSTRUCTIONS]
  0000 I_Get_Builtin 0x00000001 (00000001)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_Get_Global 0x00000000 (00000000)
  0017 I_Constant 0x00000000 (00000000)
  0022 I_Call 1
  0027 I_Pop

  $ NO_COLOR="1" print_instructions . BuiltinFunctionStringSub
  [CONSTANTS]
  0x00000000 (00000000) : "Hello, World!"
  0x00000001 (00000001) : 7
  0x00000002 (00000002) : 5
  
  [INSTRUCTIONS]
  0000 I_Get_Builtin 0x00000002 (00000002)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_Get_Global 0x00000000 (00000000)
  0017 I_Constant 0x00000000 (00000000)
  0022 I_Constant 0x00000001 (00000001)
  0027 I_Constant 0x00000002 (00000002)
  0032 I_Call 3
  0037 I_Pop
