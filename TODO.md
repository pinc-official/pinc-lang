## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- `<meta>` tags are by html spec not self closing but the current implementation requires it to be. Do we care?

- Do we want the programmer to be able to add new Tags to the cms to implement new editors?

- Do we want to provide an `library` or `module` type, for which all declarations inside are exported? This could be a
  way, we could implement a "Std" library...

  ```
    module Std {
      let concat = fun (a, b) -> { a ++ b };

      let map = fun (t, fn) -> {
        for (item in t) {
          fn(item)
        }
      };

      let default = fun (t, default) -> {
        if (!t) {
          default
        } else {
          t
        }
      };
    }

    ...

    let result = arr |> Std.default([1, 2, 3]) |> Std.map(fun (item) -> item + 2);

    ...

    (maybe even)
    use Std;
    let result = arr |> default([1, 2, 3]) |> map(fun (item) -> item + 2);
  ```

## Things

- Test wether all possible html attributes are parsing correctly. (`:`)

- Functions / Lambdas

- Pipe operator (`|` or `|>`)

- String Templates (`"Section--${alignment}"`)
