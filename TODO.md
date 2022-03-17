## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- Do we want to provide a `library` or `module` type, for which all top level declarations inside are exported? This could be a
  way, we could implement a "Std" library...

  ```
    module Std {
      let concat = fn (a, b) -> { a ++ b };

      let map = fn (t, f) -> for (item in t) f(item);

      let default = fn (t, default) -> if (!t) default else t;
    }

    ...

    let result = arr |> Std.default([1, 2, 3]) |> Std.map(fn (item) -> item + 2);
  ```

- Currently only nodes are placed into slots. Normal text (<Text>Lorem Ipsum</Text>) is ignored and not put into any slot.
  You could argue, that this should be placed into the default slot, but what do you do with the `min` and `max` restrictions?
  Does every word, every sectence or every string count as an element?
  Also transformers would become more complex, as you would get an array of nodes and strings handed. 
  And there is currently no way, to distinguish the two before you use them...
  So..is not rendering them the correct solution or not?

## Things

- Mutually Recursive Functions

- Pipe operator (`|` or `|>`)
