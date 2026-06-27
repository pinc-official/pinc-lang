  $ NO_COLOR="1" print_vm . FunctionEmpty
  

  $ NO_COLOR="1" print_vm . Function
  3

  $ NO_COLOR="1" print_vm . FunctionCurried
  1

  $ NO_COLOR="1" print_vm . FunctionScope
  97

  $ NO_COLOR="1" print_vm . FunctionArguments
  15

  $ NO_COLOR="1" print_vm . FunctionTooManyArguments
  Fatal error: exception Invalid_argument("Trying to call a function with the wrong number of arguments. Wanted 2, got 3")
  [2]

  $ NO_COLOR="1" print_vm . FunctionTooFewArguments
  Fatal error: exception Invalid_argument("Trying to call a function with the wrong number of arguments. Wanted 2, got 1")
  [2]
