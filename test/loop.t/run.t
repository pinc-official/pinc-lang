  $ print . Component
  <section>
      <h1>EXCLUSIVE RANGES</h1>
  
      0..10
      <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  
      0..0
      
  
      0..1
      <div class="item">0</div>
  
      0..-1
      
  
      -10..10
      <div class="item">-10</div>
  <div class="item">-9</div>
  <div class="item">-8</div>
  <div class="item">-7</div>
  <div class="item">-6</div>
  <div class="item">-5</div>
  <div class="item">-4</div>
  <div class="item">-3</div>
  <div class="item">-2</div>
  <div class="item">-1</div>
  <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  
      10..0
      
  
      0..10 reverse
      <div class="item">9</div>
  <div class="item">8</div>
  <div class="item">7</div>
  <div class="item">6</div>
  <div class="item">5</div>
  <div class="item">4</div>
  <div class="item">3</div>
  <div class="item">2</div>
  <div class="item">1</div>
  <div class="item">0</div>
  
  
      <h1>INCLUSIVE RANGES</h1>
  
      0...10
      <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  <div class="item">10</div>
  
      0...0
      <div class="item">0</div>
  
      0...1
      <div class="item">0</div>
  <div class="item">1</div>
  
      0...-1
      
  
      -10...10
      <div class="item">-10</div>
  <div class="item">-9</div>
  <div class="item">-8</div>
  <div class="item">-7</div>
  <div class="item">-6</div>
  <div class="item">-5</div>
  <div class="item">-4</div>
  <div class="item">-3</div>
  <div class="item">-2</div>
  <div class="item">-1</div>
  <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  <div class="item">10</div>
  
      10...0
      
  
      0...10 reverse
      <div class="item">10</div>
  <div class="item">9</div>
  <div class="item">8</div>
  <div class="item">7</div>
  <div class="item">6</div>
  <div class="item">5</div>
  <div class="item">4</div>
  <div class="item">3</div>
  <div class="item">2</div>
  <div class="item">1</div>
  <div class="item">0</div>
  
  
      <h1>ITERABLES</h1>
  
      array
      <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  
      array reverse
      <div class="item">9</div>
  <div class="item">8</div>
  <div class="item">7</div>
  <div class="item">6</div>
  <div class="item">5</div>
  <div class="item">4</div>
  <div class="item">3</div>
  <div class="item">2</div>
  <div class="item">1</div>
  <div class="item">0</div>
  
      string
      <div class="item">S</div>
  <div class="item">t</div>
  <div class="item">r</div>
  <div class="item">i</div>
  <div class="item">n</div>
  <div class="item">g</div>
  <div class="item">!</div>
  
      string array
      <div class="item">0 one</div>
  <div class="item">1 two</div>
  <div class="item">2 three</div>
  <div class="item">3 !</div>
  
      null value
      
  
  
      <h1>BREAK / CONTINUE</h1>
  
      S
  t
  r
  n
  g
  !
  
      0
  1
  2
  
    </section>

  $ print . ErrorComponent
  
  ERROR in file ./data.pi:136:24-34
  
   135 │     string reverse
   136 │     {for (c in reverse "String!") {
   137 │       <div class="item">{c}</div>
  
  Cannot loop through a string in reverse.
  This restriction might be lifted in the future, but currently reversing a string might result in unexpected behavior.
  [1]
