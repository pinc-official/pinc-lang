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
