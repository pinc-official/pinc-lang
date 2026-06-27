  $ NO_COLOR="1" print_vm . True
  true

  $ NO_COLOR="1" print_vm . False
  false

  $ NO_COLOR="1" print_vm . And
  false

  $ NO_COLOR="1" print_vm . Or
  true

  $ NO_COLOR="1" print_vm . Equal
  false

  $ NO_COLOR="1" print_vm . NotEqual
  true

  $ NO_COLOR="1" print_vm . Greater
  false

  $ NO_COLOR="1" print_vm . GreaterEqual
  false

  $ NO_COLOR="1" print_vm . Less
  true

  $ NO_COLOR="1" print_vm . LessEqual
  true

  $ NO_COLOR="1" print_vm . Not
  false
