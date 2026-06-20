  $ NO_COLOR="1" print_vm . Add
  12

  $ NO_COLOR="1" print_instructions . Add
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Add
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Sub
  2

  $ NO_COLOR="1" print_instructions . Sub
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Sub
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Div
  1.4

  $ NO_COLOR="1" print_instructions . Div
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Div
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Mul
  35

  $ NO_COLOR="1" print_instructions . Mul
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Mul
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Mod
  2

  $ NO_COLOR="1" print_instructions . Mod
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Mod
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Pow
  16807

  $ NO_COLOR="1" print_instructions . Pow
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Pow
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Math
  15

  $ NO_COLOR="1" print_instructions . Math
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Mul
  0007 I_Constant 0x0003
  0010 I_Constant 0x0004
  0013 I_Constant 0x0005
  0016 I_Constant 0x0006
  0019 I_Mul
  0020 I_Add
  0021 I_Constant 0x0007
  0024 I_Pow
  0025 I_Div
  0026 I_Add
  0027 I_Pop

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
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Equal
  0007 I_Pop

  $ NO_COLOR="1" print_vm . NotEqual
  true

  $ NO_COLOR="1" print_instructions . NotEqual
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Not_Equal
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Greater
  false

  $ NO_COLOR="1" print_instructions . Greater
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Greater
  0007 I_Pop

  $ NO_COLOR="1" print_vm . GreaterEqual
  false

  $ NO_COLOR="1" print_instructions . GreaterEqual
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Greater_Equal
  0007 I_Pop

  $ NO_COLOR="1" print_vm . Less
  true

  $ NO_COLOR="1" print_instructions . Less
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Less
  0007 I_Pop

  $ NO_COLOR="1" print_vm . LessEqual
  true

  $ NO_COLOR="1" print_instructions . LessEqual
  0000 I_Constant 0x0001
  0003 I_Constant 0x0002
  0006 I_Less_Equal
  0007 I_Pop
