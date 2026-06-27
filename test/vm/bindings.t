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
