  $ NO_COLOR="1" print_vm . Loop
  0 2 6 12 20

  $ NO_COLOR="1" print_vm . LoopNested
  1 3 4 6 7 9

  $ NO_COLOR="1" print_vm . LoopContinue
  2 4 6 8

  $ NO_COLOR="1" print_vm . LoopBreak
  1 2 3

  $ NO_COLOR="1" print_vm . LoopBreakNested
  1 2 3 4 5 6 7 8 1 2 3
