  $ NO_COLOR="1" print_instructions . Let
  [CONSTANTS]
  0x00000000 (00000000) : 1
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_Get_Global 0x00000000 (00000000)
  0017 I_Set_Global 0x00000001 (00000001)
  0022 I_Null
  0023 I_Pop
  0024 I_Get_Global 0x00000001 (00000001)
  0029 I_Pop


  $ NO_COLOR="1" print_instructions . Shadowing
  [CONSTANTS]
  0x00000000 (00000000) : 1
  0x00000001 (00000001) : 1
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_Get_Global 0x00000000 (00000000)
  0017 I_Constant 0x00000001 (00000001)
  0022 I_Add
  0023 I_Set_Global 0x00000001 (00000001)
  0028 I_Null
  0029 I_Pop
  0030 I_Get_Global 0x00000001 (00000001)
  0035 I_Pop


  $ NO_COLOR="1" print_instructions . UnboundIdentifier
  
  ERROR in file ./bindings.pi:8:13-16
  
     7 │ component UnboundIdentifier {
     8 │   let two = one;
       │             ^^^ 
     9 │   two
  
  Unbound identifier `one`
  [1]


  $ NO_COLOR="1" print_instructions . LocalBindings
  [CONSTANTS]
  0x00000000 (00000000) : 21
  0x00000001 (00000001) : <FUNCTION> [
    0000 I_Constant 0x00000000 (00000000)
    0005 I_Set_Local 0x00000000 (00000000)
    0010 I_Null
    0011 I_Pop
    0012 I_Get_Local 0x00000000 (00000000)
    0017 I_Return
  ]
  
  [INSTRUCTIONS]
  0000 I_Closure 0x00000001 (00000001) (free variables: 0)
  0009 I_Set_Global 0x00000000 (00000000)
  0014 I_Null
  0015 I_Pop
  0016 I_Get_Global 0x00000000 (00000000)
  0021 I_Call 0
  0026 I_Pop

  $ NO_COLOR="1" print_instructions . MutableBindings
  [CONSTANTS]
  0x00000000 (00000000) : "wrong"
  0x00000001 (00000001) : "correct"
  0x00000002 (00000002) : "wrong"
  0x00000003 (00000003) : <FUNCTION> [
    0000 I_Get_Global 0x00000001 (00000001)
    0005 I_Return
  ]
  0x00000004 (00000004) : "correct"
  0x00000005 (00000005) : "correct"
  0x00000006 (00000006) : <FUNCTION> [
    0000 I_Get_Global 0x00000003 (00000003)
    0005 I_Return
  ]
  0x00000007 (00000007) : "wrong"
  0x00000008 (00000008) : "wrong"
  0x00000009 (00000009) : 3
  
  [INSTRUCTIONS]
  0000 I_Constant 0x00000000 (00000000)
  0005 I_Set_Global 0x00000000 (00000000)
  0010 I_Null
  0011 I_Pop
  0012 I_True
  0013 I_Jump_If_False 0x0000000A (00000010)
  0018 I_Constant 0x00000001 (00000001)
  0023 I_Set_Global 0x00000000 (00000000)
  0028 I_Null
  0029 I_Jump 0x0000000B (00000011)
  0034 I_Null
  0035 I_Pop
  0036 I_Constant 0x00000002 (00000002)
  0041 I_Set_Global 0x00000001 (00000001)
  0046 I_Null
  0047 I_Pop
  0048 I_Closure 0x00000003 (00000003) (free variables: 0)
  0057 I_Set_Global 0x00000002 (00000002)
  0062 I_Null
  0063 I_Pop
  0064 I_Constant 0x00000004 (00000004)
  0069 I_Set_Global 0x00000001 (00000001)
  0074 I_Null
  0075 I_Pop
  0076 I_Constant 0x00000005 (00000005)
  0081 I_Set_Global 0x00000003 (00000003)
  0086 I_Null
  0087 I_Pop
  0088 I_Closure 0x00000006 (00000006) (free variables: 0)
  0097 I_Set_Global 0x00000004 (00000004)
  0102 I_Null
  0103 I_Pop
  0104 I_Constant 0x00000007 (00000007)
  0109 I_Set_Global 0x00000005 (00000005)
  0114 I_Null
  0115 I_Pop
  0116 I_Constant 0x00000008 (00000008)
  0121 I_Set_Global 0x00000005 (00000005)
  0126 I_Null
  0127 I_Pop
  0128 I_Get_Global 0x00000000 (00000000)
  0133 I_Get_Global 0x00000002 (00000002)
  0138 I_Call 0
  0143 I_Get_Global 0x00000004 (00000004)
  0148 I_Call 0
  0153 I_Constant 0x00000009 (00000009)
  0158 I_Array
  0159 I_Pop

  $ NO_COLOR="1" print_instructions . WrongMutation
  
  ERROR in file ./bindings.pi:48:5-6
  
    47 │   if (true) {
    48 │     a := "error!";
       │     ^             
    49 │   };
  
  Trying to update a non mutable variable `a`.
  [1]
