  $ esy x print . Component
  <section>
      max(1, 5) = 5
      min(1, 5) = 1
      add_curry(1)(5) = 6
      fibonacci(9) = 34
      factorial(5) = 120
      is_even(10) = true
      is_odd(10) = false
  
      1 |> max(5) |> factorial = 120
      1 |> add_curry(5)() = 6
      5 |> factorial = 120
  
      Map:
      1
  4
  9
  16
      
      Map Pipe:
      1
  4
  9
  16
    </section>

