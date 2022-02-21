## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- Do we want to provide an `library` or `module` type, for which all declarations inside are exported? This could be a
  way, we could implement a "Std" library...

  ```
    module Std {
      let concat = fn (a, b) -> { a ++ b };

      let map = fn (t, f) -> for (item in t) f(item);

      let default = fn (t, default) -> if (!t) default else t;
    }

    ...

    let result = arr |> Std.default([1, 2, 3]) |> Std.map(fn (item) -> item + 2);

    ...

    (maybe even)
    use Std;
    let result = arr |> default([1, 2, 3]) |> map(fn (item) -> item + 2);
  ```

## Things

- Functions / Lambdas

- Pipe operator (`|` or `|>`)

- Cleanup String Templates!!
