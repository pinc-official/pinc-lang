  $ NO_COLOR="1" print . Component
  <div class="SlotProvider">
  <area class="slot_default"><div>Hello Default</div></area>
  <area class="slot_restricted"><div class="SubComponent" data-foo="9">
  Hello, SubComponent!
  
  234567891011
  
  JAP
  
  Something
  Some Constant
  
  NOTHING!!!
  </div></area>
  <area class="multi"><div><span class="item" slot="multi">Yaaay! 0</span></div><div><span class="item" slot="multi">Yaaay! 1</span></div><div><span class="item" slot="multi">Yaaay! 2</span></div><div><span class="item" slot="multi">Yaaay! 3</span></div><div><span class="item" slot="multi">Yaaay! 4</span></div><div><span class="item" slot="multi">Yaaay! 5</span></div><div><span class="item" slot="multi">Yaaay! 6</span></div><div><span class="item" slot="multi">Yaaay! 7</span></div><div><span class="item" slot="multi">Yaaay! 8</span></div><div><span class="item" slot="multi">Yaaay! 9</span></div></area>
  <area class="slot_nothing"></area>
  </div>

  $ NO_COLOR="1" print . ErrorComponent
  
  ERROR in file ./data.pi:38:5-44
  
    37 │   <SlotProvider>
    38 │     <div slot="nothing">Hello Default</div>
       │     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    39 │   </SlotProvider>
  
  Child with tag `div` may not be used inside this #Slot. 
  It has an empty array set as constrints, which leads to nothing being allowed to be placed inside.
  [1]

  $ pincfmt ./data.pi
  component Component(
    label: "Section",
    icon: "/images/icons/group.svg",
    group: "Structure",
  ) {
    let array = for (i in 0..10) {
      {
        i: i,
      }
    };
    let record = {
      a: 10,
      c: "Something",
    };
  
    <SlotProvider>
      <div>Hello Default</div>
  
      <div slot="nope">I am not rendered!</div>
  
      <SubComponent
        slot="restricted"
        content="Hello, SubComponent!"
        num={
          3
        }
        array_things={
          array
        }
        record={
          record
        }
      />
  
      {if (record.a > 9) { <span slot="nope">Yaaay!</span> }}
  
      {for (i in 0..record.a) { <span slot="multi">Yaaay! {i}</span> }}
    </SlotProvider>
  }
  
  component ErrorComponent(
    label: "Section",
    icon: "/images/icons/group.svg",
    group: "Structure",
  ) {
    <SlotProvider>
      <div slot="nothing">Hello Default</div>
    </SlotProvider>
  }
  

  $ pincfmt ./SlotProvider.pi
  component SlotProvider {
    let slot_default = #Slot(key: "");
  
    let restricted = #Slot(max: 1, constraints: [SubComponent]);
  
    let multi = #Slot :: fn (els) -> for (el in els) {
      <div>{
          el @@ {
            class: "item",
          }
        }</div>
    };
  
    let slot_nothing = #Slot(key: "nothing", constraints: []);
  
    <div class="SlotProvider">
      <area class="slot_default">{slot_default}</area>
      <area class="slot_restricted">{restricted}</area>
      <area class="multi">{multi}</area>
      <area class="slot_nothing">{slot_nothing}</area>
    </div>
  }
  

  $ pincfmt ./SubComponent.pi
  component SubComponent {
    let content = #String;
  
    let shouter = #String :: fn (val) -> if (val) val ++ "!!" else "NOTHING!!!";
  
    let num = #Int :: fn (val) -> val ** 2;
  
    let array_things = #Array(
      of: #Record(
        of: {
          i: #Int :: fn (i) -> i + 2,
        },
      ),
    );
  
    let record = #Record(
      of: {
        a: #Int(default: 12) :: fn (val) -> if (val == 12) "NEEEEIN" else "JAP",
        b?: #Boolean,
        c: #String,
        d: "Some Constant",
      },
    );
  
    <div class="SubComponent" data-foo={num}>
      {content}
  
      {array_things |> Base.Array.map(fn (r) -> r.i)}
  
      {record.a}
      {record.b}
      {record.c}
      {record.d}
  
      {shouter}
    </div>
  }
  
