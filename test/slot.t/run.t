  $ print . Component
  <div class="SlotProvider">
      <area class="slot_default"><div>Hello Default</div></area>
      <area class="slot_restricted"><div class="SubComponent" data-foo="9">
      Hello, SubComponent!
  
      2
  3
  4
  5
  6
  7
  8
  9
  10
  11
  
      JAP
      
      Something
      Some Constant
      
      NOTHING!!!
    </div></area>
      <area class="multi"><div><span class="item" slot="multi">Yaaay! 0</span></div>
  <div><span class="item" slot="multi">Yaaay! 1</span></div>
  <div><span class="item" slot="multi">Yaaay! 2</span></div>
  <div><span class="item" slot="multi">Yaaay! 3</span></div>
  <div><span class="item" slot="multi">Yaaay! 4</span></div>
  <div><span class="item" slot="multi">Yaaay! 5</span></div>
  <div><span class="item" slot="multi">Yaaay! 6</span></div>
  <div><span class="item" slot="multi">Yaaay! 7</span></div>
  <div><span class="item" slot="multi">Yaaay! 8</span></div>
  <div><span class="item" slot="multi">Yaaay! 9</span></div></area>
      <area class="slot_nothing"></area>
    </div>

  $ print . ErrorComponent
  
  ERROR in file ./data.pi:38:5-39:3
  
  37 │   <SlotProvider>
  38 │     <div slot="nothing">Hello Default</div>
  39 │   </SlotProvider>
  40 │ }
  
  Child with tag `div` may not be used inside this #Slot. 
  It has an empty array set as constrints, which leads to nothing being allowed to be placed inside.
  [1]
