  $ NO_COLOR="1" print . Component
  <section class="Section">
  
  <h2 class="HeadlineSecondary ">
  Hello, Headline Secondary!
  </h2>
  <div>
  <ul>
  <li>0</li><li>1</li><li>2</li><li>3</li>
  </ul><ul>
  <li>0</li><li>1</li><li>2</li><li>3</li>
  </ul><ul>
  <li>0</li><li>1</li><li>2</li><li>3</li>
  </ul>
  </div>
  </section>

  $ NO_COLOR="1" print . Foo
  
  ERROR 
  
  Declaration with name `Foo` was not found.
  [1]

  $ pincfmt ./data.pi
  component Component(
    label: "Section",
    icon: "/images/icons/group.svg",
    group: "Structure",
  ) {
    let array = for (i in 0..10) {
      i
    };
    let record = {
      a: 10,
      c: "Something",
    };
  
    let matrix = for (i in 0...2) {
      0...3
    };
  
    /* COMMENT with nested /* COMMENT!! */ */
  
    <section class="Section">
      {/* <span> {config} </span> */}
      <HeadlineSecondary
        tag="h2"
        data={
          {
            text: "Hello, Headline Secondary!",
          }
        }
      />
      <Matrix matrix={matrix} />
    </section>
  }
