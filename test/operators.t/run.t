  $ esy x print ./data.fe Section
  <div>
  ![1] = false
  ![] = true
  !!"" = false
  !true = false
  -1 = -1
  -2.5 = -2.5
  
  true == true = true
  true != true = false
  true && true = true
  true && false = false
  true || false = true
  false || false = false
  
  1 > 2 = false
  1 < 2 = true
  1 <= 1 = true
  2 <= 1 = false
  1 >= 1 = true
  
  [1] == [1] = true
  [1] == [2] = false
  [1, 2] >= [5] = true
  [1] >= [1,2,3] = false
  
  1 + 1 = 2
  1.3 + 1.5 = 2.8
  1 + 1.2 = 2.2
  2.73 + 3 = 5.73
  
  1 - 1 = 0
  1.3 - 1.5 = -0.2
  1 - 1.2 = -0.2
  2.73 - 3 = -0.27
  
  1 * 1 = 1
  1.3 * 1.5 = 1.95
  1 * 1.2 = 1.2
  2.73 * 3 = 8.19
  
  1 / 1 = 1
  1.3 / 1.5 = 0.866666666667
  1 / 1.2 = 0.833333333333
  2.73 / 3 = 0.91
  
  3 ** 1 = 3
  3 ** 2 = 9
  3 ** 3 = 27
  2.73 ** 3 = 20.346417
  
  "a" ++ "b" = ab
  "a" ++ "b" ++ "c" = abc
  </div>

