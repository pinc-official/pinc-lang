  $ NO_COLOR="1" print . NotExistingDefinition
  
  ERROR 
  
  Declaration with name `NotExistingDefinition` was not found.
  [1]

  $ NO_COLOR="1" print . BadTransformer_Arity
  
  ERROR in file ./tag_transformer_arity.pi:6:25-37
  
     5 │ component BadTransformer_Arity_Child {
     6 │   let text = #String :: fn (a, b) -> Base_String.uppercase(a);
       │                         ^^^^^^^^^^^^                          
     7 │ 
  
  A transformer has to accept exactly one argument (the tag value).
  Here it was provided 2.
  [1]
  $ NO_COLOR="1" print . BadTransformer_Typ
  
  ERROR in file ./tag_transformer_typ.pi:6:25-41
  
     5 │ component BadTransformer_Typ_Child {
     6 │   let text = #String :: "not a function";
       │                         ^^^^^^^^^^^^^^^^ 
     7 │ 
  
  Trying to assign a non function value to a transformer.
  [1]
