# PiNC

## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- Selection / Enum ... can be defined in "userland" by the cms / playground. Do we want to move it into the core
  language?

## TODOs

- Replace `failwith` calls with Diagnostics.error

- Comments inside html are currently evaluated

  ```
  <html lang="de">
    <body>
      // <footer> {config} </footer>

      <script src="/js/main.bundle.js" />
      {#CreatePortal(key: "javascripts")}
    </body>
  </html>
  ```

- Rewrite strings to be an array of chars and make chars a separate value type

- Add named parameters to function application

  ```
    let add = fn (a, b) -> {
      a + b
    };

    // The following two would be the same:
    add(1, 3);
    add(b=3, a=1);
  ```

- Add default parameters to function declatation

  ```
    let add = fn (a, b=10) -> {
      a + b
    };

    add(1, 3); // 4
    add(1); // 11
  ```

- Remove catchall cases in switch stmts

- Incremental

- Type checker

- Type inference

- Unicode support for identifiers

- Implement a "Std" library

  ```
    library Std {
      use Array = Std__Array;
      use String = Std__String;

      let default = fn (t, default) -> if (!t) default else t;
    }

    library Std__Array {
      let map = fn (t, f) -> for (item in t) f(item);
    }

    library Std__String {
      let concat = fn (a, b) -> { a ++ b };
    }

    ...

    let result = arr |> Std.default([1, 2, 3]) |> Std.Array.map(fn (item) -> item + 2);
  ```

- Implement `store` as a collection of tags:
  ```
  store Products(
    label: "Products",
    icon: "/images/icons/package.svg",
  ) {
    foo: #String(label: "Foo"),

    products: #Array(label: "Products", of: #Record(label: "Product", of: {
      name: #String(label: "Name"),
      image: #Image(label: "Image"),
      code: #String(label: "Product Code"),
      supply: #String(label: "Scope of delivery", rows: 3),
      tags: #Array(label: "Tags", of: #String(label: "Tag")),
    }));
  }
  ```