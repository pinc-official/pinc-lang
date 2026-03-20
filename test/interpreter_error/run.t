  $ NO_COLOR="1" print . NotExistingDefinition
  
  TYPE ERROR in file ./use_non_library.pi:2:7-18
  
     1 │ component UseNonLibrary {
     2 │   use NotALibrary;
       │       ^^^^^^^^^^^ 
     3 │ }
  
  Unbound declaration `NotALibrary`
  [1]

  $ NO_COLOR="1" print . UseNonLibrary
  
  TYPE ERROR in file ./use_non_library.pi:2:7-18
  
     1 │ component UseNonLibrary {
     2 │   use NotALibrary;
       │       ^^^^^^^^^^^ 
     3 │ }
  
  Unbound declaration `NotALibrary`
  [1]

  $ NO_COLOR="1" print . BadTransformer_Arity
  
  TYPE ERROR in file ./use_non_library.pi:2:7-18
  
     1 │ component UseNonLibrary {
     2 │   use NotALibrary;
       │       ^^^^^^^^^^^ 
     3 │ }
  
  Unbound declaration `NotALibrary`
  [1]
  $ NO_COLOR="1" print . BadTransformer_Typ
  
  TYPE ERROR in file ./use_non_library.pi:2:7-18
  
     1 │ component UseNonLibrary {
     2 │   use NotALibrary;
       │       ^^^^^^^^^^^ 
     3 │ }
  
  Unbound declaration `NotALibrary`
  [1]
