  $ NO_COLOR="1" print . Docs
  <span></span>
  <div class="foo"></div>
  
  <ul>
  <li>apples</li><li>oranges</li><li>bannanas</li><li>melons</li>
  </ul>
  <p>
  Aliqua in elit sunt in anim culpa nostrud elit.
  Elit voluptate nisi laborum est eu esse non exercitation minim.
  Nisi ea enim minim quis.
  </p>
  

  $ pincfmt ./data.pi
  page Docs(label: "Docs", icon: "/images/icons/page-docs.svg") {
    let fruits = ["apples", "oranges", "bannanas", "melons"];
  
    <>
      <span />
      <>
        <div class="foo" />
      </>
      <ul>
        {for (fruit in fruits) { <li>{fruit}</li> }}
      </ul>
      <p>
        Aliqua in elit sunt in anim culpa nostrud elit.
        Elit voluptate nisi laborum est eu esse non exercitation minim.
        Nisi ea enim minim quis.
      </p>
    </>
  }
  
