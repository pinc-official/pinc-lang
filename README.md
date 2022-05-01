# PiNC

## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- Selection / Enum ... can be defined in "userland" by the cms / playground. Do we want to move it into the core
  language?

## TODOs till feature completion

- Remove catchall cases in switch stmts

- Cache tag values in Portal_Collection mode

## TODOs after feature completion:

- Generate better error messages

- Incremental

- Type checker

- Type inference

- Unicode support for identifiers

- Rewrite strings to be an array of chars and make chars a separate value type

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
