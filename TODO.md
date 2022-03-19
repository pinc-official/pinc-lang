## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- Currently only nodes are placed into slots. Normal text (<Text>Lorem Ipsum</Text>) is ignored and not put into any slot.
  You could argue, that this should be placed into the default slot, but what do you do with the `min` and `max` restrictions?
  Does every word, every sectence or every string count as an element?
  Also transformers would become more complex, as you would get an array of nodes and strings handed. 
  And there is currently no way, to distinguish the two before you use them...
  So..is not rendering them the correct solution or not?

## TODOs till feature completion

- Mutually Recursive Functions

- Portals

- Context

- A `library` type, for which all top level declarations inside are exported.

- A way to include libraries into the namespace of another library. (See below for an example)

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

## TODOs after feature completion:

- Type checker

- Type inference