## Questions

- What to do with `<!DOCTYPE html>`? Do we want to parse it or just fail compiling with an error message that this is
  not needed?

- `<meta>` tags are by html spec not self closing but the current implementation requires it to be. Do we care?

- Do we need Objects / Maps? (`{ foo: "bar" }`)

- Do we want the programmer to be able to add new Tags to the cms to implement new editors?

## Things

- Test wether all possible html attributes are parsing correctly. (`:`)

- Keep whitespace in template mode (for compat with <pre> tag)

- Functions / Lambdas

- Pipe operator

- `.` operator for object access

- String Templates (`"Section--${alignment}"`)
