  $ print ./empty_template_expression.pi Component
  <div class="Section">
      
    </div>
  
  WARNING in file ./empty_template_expression.pi:4:5-7
  
  3 │   <div class="Section">
  4 │     {}
  5 │   </div>
  
  Expected to see an expression between these braces. 
  This is currently not doing anything, so you can safely remove it.

  $ print ./tag_transformer_function.pi Component
  
  ERROR in file ./tag_transformer_function.pi:2:22-31
  
  1 │ component Component {
  2 │   let fail = #String :: _ -> ;
  3 │ 
  
  This tag transformer does not have a valid body.
  [1]

  $ print ./let_without_expression.pi Component
  
  ERROR in file ./let_without_expression.pi:2:3-15
  
  1 │ component Component {
  2 │   let fail = ;
  3 │ 
  
  Expected expression as right hand side of let declaration
  [1]
  $ print ./mutation_without_expression.pi Component
  
  ERROR in file ./mutation_without_expression.pi:3:3-12
  
  2 │   let mutable fail = "";
  3 │   fail := ;
  4 │ 
  
  Expected expression as right hand side of mutation statement
  [1]
