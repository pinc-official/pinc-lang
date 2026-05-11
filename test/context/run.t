  $ NO_COLOR="1" print . Component
  <section class="Section">
  <h3 class="HeadlineSecondary ">
  Hello, Headline Secondary!
  </h3>
  <div class="Sub Sub--grey">
  <p>Faucibus mus velit ut iaculis cubilia taciti conubia, ad magna sem mattis tincidunt eleifend, egestas eget neque curae molestie donec. Magna ipsum netus euismod vitae justo dictum adipiscing lobortis, posuere tortor penatibus tempus laoreet habitant orci, turpis blandit nullam ornare fames nostra sed.</p>
  <p></p>
  </div>
  </section>

  $ pincfmt ./data.pi
  component Component(
    label: "Section",
    icon: "/images/icons/group.svg",
    group: "Structure",
  ) {
    #SetContext(key: "background", value: "grey");
  
    <section class="Section">
      <HeadlineSecondary text="Hello, Headline Secondary!" />
      <SubComponent />
    </section>
  }

  $ pincfmt ./HeadlineSecondary.pi
  component HeadlineSecondary {
    #SetContext(key: "test", value: "should never be displayed!");
  
    let class? = #String :: fn (value) -> if (!value) "";
  
    let tag? = #String :: fn (value) -> if (!value) "h3";
    let data = {
      text: #String,
      subheadline?: #String,
    };
  
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

  $ pincfmt ./SubComponent.pi
  component SubComponent {
    let background = #GetContext;
    let test? = #GetContext;
  
    let backgroundClass = if (background == "grey") "Sub--grey" else "";
  
    <div class="Sub $(backgroundClass)">
      <p>Faucibus mus velit ut iaculis cubilia taciti conubia, ad magna sem mattis tincidunt eleifend, egestas eget neque curae molestie donec. Magna ipsum netus euismod vitae justo dictum adipiscing lobortis, posuere tortor penatibus tempus laoreet habitant orci, turpis blandit nullam ornare fames nostra sed.</p>
      <p>{test}</p>
    </div>
  }
