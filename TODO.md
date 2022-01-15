- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- `<meta>` tags are by html spec not self closing but the current implementation requires it to be. Do we care?

- Keep whitespace in template mode (for compat with <pre> tag)

- Test wether all possible html attributes are parsing correctly. (`:`)

## Parser

- Operator precedence

- for in Expression: Needs testing
