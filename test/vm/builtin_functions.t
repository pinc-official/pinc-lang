  $ NO_COLOR="1" print_vm . BuiltinFunctionWrongParameters
  
  ERROR in file ./builtin_functions.pi:2:16-50
  
     1 │ component BuiltinFunctionWrongParameters {
     2 │   let length = fn (a, b) -> %%pinc_array_length%%;
       │                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
     3 │ 
  
  External function %%pinc_array_length%% expected 1 parameters, but got 2
  [1]

  $ NO_COLOR="1" print_vm . BuiltinFunctionArrayLength
  4

  $ NO_COLOR="1" print_vm . BuiltinFunctionStringLength
  13

  $ NO_COLOR="1" print_vm . BuiltinFunctionStringSub
  World
