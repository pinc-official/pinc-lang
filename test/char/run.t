  $ NO_COLOR="1" print . C
  
  TYPE ERROR in file ./data.pi:14:24-26
  
    13 │     char + 1: {char + 1}
    14 │     char < 32: {char < 32}
       │                        ^^ 
    15 │     char > 32: {char > 32}
  
  Types do not match. Expected `char` got `int`.
  [1]

