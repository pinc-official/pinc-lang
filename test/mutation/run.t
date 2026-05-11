  $ NO_COLOR="1" print . Component
  <section>
  a: mutated
  b: mutated
  print_c(): mutated
  print_d(): initial
  </section>

  $ pincfmt ./data.pi
  component Component {
    let mutable a = "initial";
    a := "mutated";
  
    let mutable b = "initial";
    if (a == "mutated") {
      b := "mutated";
    };
  
    let mutable c = "initial";
    let print_c = fn () -> c;
    c := "mutated";
  
    let mutable d = "initial";
    let print_d = fn () -> d;
    let mutable d = "initial2";
    d := "mutated";
  
    <section>
      a: {a}
      b: {b}
      print_c(): {print_c()}
      print_d(): {print_d()}
    </section>
  }
  
