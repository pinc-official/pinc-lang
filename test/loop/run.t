  $ NO_COLOR="1" print . Component
  <section>
  <h1>EXCLUSIVE RANGES</h1>
  
  0..10
  <div class="item">0</div><div class="item">1</div><div class="item">2</div><div class="item">3</div><div class="item">4</div><div class="item">5</div><div class="item">6</div><div class="item">7</div><div class="item">8</div><div class="item">9</div>
  
  0..0
  
  
  0..1
  <div class="item">0</div>
  
  0..-1
  
  
  -10..10
  <div class="item">-10</div><div class="item">-9</div><div class="item">-8</div><div class="item">-7</div><div class="item">-6</div><div class="item">-5</div><div class="item">-4</div><div class="item">-3</div><div class="item">-2</div><div class="item">-1</div><div class="item">0</div><div class="item">1</div><div class="item">2</div><div class="item">3</div><div class="item">4</div><div class="item">5</div><div class="item">6</div><div class="item">7</div><div class="item">8</div><div class="item">9</div>
  
  10..0
  
  
  0..10 reverse
  <div class="item">9</div><div class="item">8</div><div class="item">7</div><div class="item">6</div><div class="item">5</div><div class="item">4</div><div class="item">3</div><div class="item">2</div><div class="item">1</div><div class="item">0</div>
  
  
  <h1>INCLUSIVE RANGES</h1>
  
  0...10
  <div class="item">0</div><div class="item">1</div><div class="item">2</div><div class="item">3</div><div class="item">4</div><div class="item">5</div><div class="item">6</div><div class="item">7</div><div class="item">8</div><div class="item">9</div><div class="item">10</div>
  
  0...0
  <div class="item">0</div>
  
  0...1
  <div class="item">0</div><div class="item">1</div>
  
  0...-1
  
  
  -10...10
  <div class="item">-10</div><div class="item">-9</div><div class="item">-8</div><div class="item">-7</div><div class="item">-6</div><div class="item">-5</div><div class="item">-4</div><div class="item">-3</div><div class="item">-2</div><div class="item">-1</div><div class="item">0</div><div class="item">1</div><div class="item">2</div><div class="item">3</div><div class="item">4</div><div class="item">5</div><div class="item">6</div><div class="item">7</div><div class="item">8</div><div class="item">9</div><div class="item">10</div>
  
  10...0
  
  
  0...10 reverse
  <div class="item">10</div><div class="item">9</div><div class="item">8</div><div class="item">7</div><div class="item">6</div><div class="item">5</div><div class="item">4</div><div class="item">3</div><div class="item">2</div><div class="item">1</div><div class="item">0</div>
  
  
  <h1>ITERABLES</h1>
  
  array
  <div class="item">0</div><div class="item">1</div><div class="item">2</div><div class="item">3</div><div class="item">4</div><div class="item">5</div><div class="item">6</div><div class="item">7</div><div class="item">8</div><div class="item">9</div>
  
  array reverse
  <div class="item">9</div><div class="item">8</div><div class="item">7</div><div class="item">6</div><div class="item">5</div><div class="item">4</div><div class="item">3</div><div class="item">2</div><div class="item">1</div><div class="item">0</div>
  
  string
  <div class="item">S</div><div class="item">t</div><div class="item">r</div><div class="item">i</div><div class="item">n</div><div class="item">g</div><div class="item">!</div>
  
  string reverse
  <div class="item">!</div><div class="item">g</div><div class="item">è</div><div class="item">n</div><div class="item">i</div><div class="item">ä</div><div class="item">r</div><div class="item">t</div><div class="item">S</div>
  
  string array
  <div class="item">0 one</div><div class="item">1 two</div><div class="item">2 three</div><div class="item">3 !</div>
  
  null value
  
  
  
  <h1>BREAK / CONTINUE</h1>
  
  Strng!
  
  012
  
  </section>

  $ pincfmt ./data.pi
  component Component {
  
    let max = 10;
  
    let array = 0..max;
  
    let string_array = ["one", "two", "three", "!"];
  
    let null_value? = if (false) "something";
  
    <section>
      <h1>EXCLUSIVE RANGES</h1>
  
      0..10
      {for (i in 0..10) {
        <div class="item">{i}</div>
      }}
  
      0..0
      {for (i in 0..0) {
        <div class="item">{i}</div>
      }}
  
      0..1
      {for (i in 0..1) {
        <div class="item">{i}</div>
      }}
  
      0..-1
      {for (i in 0..-1) {
        <div class="item">{i}</div>
      }}
  
      -10..10
      {for (i in -10..10) {
        <div class="item">{i}</div>
      }}
  
      10..0
      {for (i in 10..0) {
        <div class="item">{i}</div>
      }}
  
      0..10 reverse
      {for (i in reverse 0..10) {
        <div class="item">{i}</div>
      }}
  
  
      <h1>INCLUSIVE RANGES</h1>
  
      0...10
      {for (i in 0...10) {
        <div class="item">{i}</div>
      }}
  
      0...0
      {for (i in 0...0) {
        <div class="item">{i}</div>
      }}
  
      0...1
      {for (i in 0...1) {
        <div class="item">{i}</div>
      }}
  
      0...-1
      {for (i in 0...-1) {
        <div class="item">{i}</div>
      }}
  
      -10...10
      {for (i in -10...10) {
        <div class="item">{i}</div>
      }}
  
      10...0
      {for (i in 10...0) {
        <div class="item">{i}</div>
      }}
  
      0...10 reverse
      {for (i in reverse 0...10) {
        <div class="item">{i}</div>
      }}
  
  
      <h1>ITERABLES</h1>
  
      array
      {for (i in array) {
        <div class="item">{i}</div>
      }}
  
      array reverse
      {for (i in reverse array) {
        <div class="item">{i}</div>
      }}
  
      string
      {for (c in "String!") {
        <div class="item">{c}</div>
      }}
  
      string reverse
      {for (c in reverse "Sträinèg!") {
        <div class="item">{c}</div>
      }}
  
      string array
      {for (index, s in string_array) {
        <div class="item">{index} {s}</div>
      }}
  
      null value
      {for (s in null_value) {
        <div class="item">{s}</div>
      }}
  
  
      <h1>BREAK / CONTINUE</h1>
  
      {for (c in "String!") {
        if (c == 'i') {
          continue;
        };
  
        c
      }}
  
      {for (i in 0..5) {
        if (i == 3) {
          break;
        };
  
        i
      }}
  
    </section>
  }
