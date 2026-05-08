  $ NO_COLOR="1" print . Main
  <html>
  <head>
  <link href="path/to/base.css" rel="stylesheet" /><link href="path/to/text.css" rel="stylesheet" /><link href="path/to/section.css" rel="stylesheet" />
  </head>
  <body>
  <section class="Ah"><p>LOREM!</p><p>Ipsum!</p></section>
  </body>
  </html>

  $ pincfmt ./main.pi
  component Main() {
    #Portal(key: "stylesheets", push: <link rel="stylesheet" href="path/to/base.css" />)
  
    <html>
      <Head />
      <body>
        <Section foo="Ah">
          <Text text="LOREM!" />
          <Text text="Ipsum!" />
        </Section>
      </body>
    </html>
  }
  
  component Head() {
    let stylesheets = #CreatePortal :: Base.Array.unique;
  
    <head>
      {stylesheets}
    </head>
  }
  
  component Section() {
    #Portal(key: "stylesheets", push: <link rel="stylesheet" href="path/to/section.css" />)
  
    let foo = #String;
    let content = #Slot(key: "");
  
    <section class={foo}>{content}</section>
  }
  
  component Text() {
    #Portal(key: "stylesheets", push: <link rel="stylesheet" href="path/to/text.css" />)
  
    let text = #String;
  
    <p>{text}</p>
  }
