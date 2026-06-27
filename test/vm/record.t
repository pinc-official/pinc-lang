  $ NO_COLOR="1" print_vm . Record
  1
  foo
  true
  3.1415

  $ NO_COLOR="1" print_vm . RecordEmpty
  

  $ NO_COLOR="1" print_vm . RecordNested
  1
  foo
  123
  321
  1 2 3 4 5 6 7 8
  true

  $ NO_COLOR="1" print_vm . RecordNested
  1
  foo
  123
  321
  1 2 3 4 5 6 7 8
  true

  $ NO_COLOR="1" print_vm . RecordAccessDot
  123

  $ NO_COLOR="1" print_vm . RecordAccessBracket
  123
