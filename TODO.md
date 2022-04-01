## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

## TODOs till feature completion

- Portals

- Make it possible to define any expression inside the `#Record(of: { ... });` attributes.

- Selection / Enum _Maybe we want to add the ability for the "user" (cms / playground) to define this._

- A `library` type, for which all top level declarations inside are exported.

- A way to include libraries into the namespace of another library. (See below for an example)

## TODOs after feature completion:

- Type checker

- Type inference

- Implement a "Std" library

  ```
    library Std {
      include Array = Std__Array;
      include String = Std__String;

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
