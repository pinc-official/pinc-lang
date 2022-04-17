# PiNC

## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- Selection / Enum ... can be defined in "userland" by the cms / playground. Do we want to move it into the core
  language?

## TODOs till feature completion

### Portals

- Portals may not be used inside if conditions. If you were able to do that, the branches (and possibly tags) you reach,
  would be different on both renders. This wouldn't be an issue with _new_ tags we encounter on the second render, but
  it would be a problem if you "remove" tags, you encountered on the first run.

- We will have two completely separate render steps from the root.
  - The first render will have a flag set, to collect all portal values and cache all tags values (so we do not call
    tag-listeners multiple times). On this render, the portal will behave like its value is empty (`[]`).
  - If we didn't encouner any portals or all portals still have an empty value after the first render, we can use this
    result. If this is not the case, we need to issue a second render.
  - On the second render, the flag is not set and we will render normally with all values collected on the previous
    render.
  - Because portals are not able to be used inside any branching conditions, the two renders _should_ go through the
    same code paths...

## TODOs after feature completion:

- Generate better error messages

- Incremental

- Type checker

- Type inference

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
