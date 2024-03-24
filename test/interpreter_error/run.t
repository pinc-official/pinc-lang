  $ NO_COLOR="1" print . NotExistingDefinition
  
  ERROR 
  
  Declaration with name `NotExistingDefinition` was not found.
  [1]

  $ NO_COLOR="1" print . UseNonLibrary
  
  ERROR in file ./data.pi:2:7-19
  
     1 │ component UseNonLibrary {
     2 │   use NotALibrary;
       │       ^^^^^^^^^^^^
     3 │ }
  
  Attempted to use a non library definition. 
  Expected to see a Library at the right hand side of the `use` statement.
  [1]
