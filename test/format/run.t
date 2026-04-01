  $ pincfmt ./data.pi
  page Docs(label: "Docs", icon: "/images/icons/page-docs.svg") {
    let fruits = ["apples", "oranges", "bannanas", "melons"];
    <>
      <span />
      <>
        <div class="foo" />
      <>
      <ul>
        {}
      </ul>
      <p>
        Aliqua in elit sunt in anim culpa nostrud elit.
        Elit voluptate nisi laborum est eu esse non exercitation minim.
        Nisi ea enim minim quis.
      </p>
    <>
  }
  
  component Component(
    label: "Long Name of $(c)",
    icon: "/images/icons/page-docs.svg",
    groups: ["Some Group", "Another Group", "And another group"],
  ) {
    let fruits = [
      "apples",
      "oranges",
      "bannanas",
      "melons",
      "and a bunch more very healthy things you can eat",
    ];
    <>
      <span />
      <>
        <div class="foo" />
      <>
      <ul>
        {}
      </ul>
      <p>
        Aliqua in elit sunt in anim culpa nostrud elit.
        Elit voluptate nisi laborum est eu esse non exercitation minim.
        Nisi ea enim minim quis.
      </p>
    <>
  }
  
  library NoAttributes() {
    <div />
  }
  
  component Mutation() {
    let mutable a = "initial";
    a := "mutated";
    let mutable b = "initial";
  
    let mutable c = "initial";
    let print_c = fn () -> c;
    c := "mutated";
    let mutable d = "initial";
    let print_d = fn () -> d;
    let mutable d = "initial2";
    d := "mutated";
    <section class="foo" data-test="bar">
      a: {a}
      b: {b}
      print_c(): {}
      print_d(): {}
    </section>
  }
