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
  

  $ pincfmt ./Matrix.pi
  component Matrix {
    let matrix = #Array(of: #Array(of: #Int));
  
    <div>
      {
        for (row in matrix) {
          <ul>
            {for (col in row) { <li>{col}</li> }}
          </ul>
        }
      }
    </div>
  }
  

  $ pincfmt ./Sub/SubComponent.pi
  component Subheadline {
    let text = #String;
  
    <span>{text}</span>
  }
  
  component HeadlineSecondary {
    let class? = #String;
  
    let tag? = #String :: fn (value) -> value
      |> Base_Fn.default("h3");
  
    let data = #Record(
      of: {
        text: if (tag == "h2") #String else "No no no ... make h2 when you want text",
        subheadline?: if (tag == "h2") #String,
      },
    );
  
    let class = "HeadlineSecondary $(class)";
  
    let content = <>
      {if (data.subheadline) <Subheadline text={data.subheadline} />}
      {data.text}
    </>;
  
    if (tag == "h1") {
      <h1 class={class}>{content}</h1>
    } else if (tag == "h2") {
      <h2 class={class}>{content}</h2>
    } else if (tag == "h3") {
      <h3 class={class}>{content}</h3>
    } else if (tag == "h4") {
      <h4 class={class}>{content}</h4>
    } else if (tag == "h5") {
      <h5 class={class}>{content}</h5>
    } else if (tag == "h6") {
      <h6 class={class}>{content}</h6>
    } else {
      "INVALID TAG!"
    }
  }
  
