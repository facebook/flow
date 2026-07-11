Implement the pieces used by the `App` component already in `main.js`: a `FieldSpec` type, a `serverRenderableLabels` function, and two components named `Sparkline` and `Avatar`.

A `FieldSpec` has a `label` (string) and a `render` value describing what draws the field. A `render` value can be either an HTML tag name (such as `'input'` or `'textarea'`) or a custom React component; type the `render` field so both are accepted.

`serverRenderableLabels` takes a read-only array of field specs and returns, in input order, the labels of only the fields whose `render` is a plain HTML tag name. (Those fields can be pre-rendered on the server; the ones backed by a component cannot.)

`Sparkline` and `Avatar` are components that each take no props.
