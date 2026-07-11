Implement the `TextInput` and `LabeledField` components used by the `App` component already in `main.js`.

`TextInput` takes a `value: string` and exposes its underlying `<input>` element through a `ref` parameter (render a read-only `<input>`).

`LabeledField` takes a `label: string`, a `value: string`, and a `ref`. It renders the `label` beside a `TextInput` and forwards its own `ref` straight through to that `TextInput`, so a caller holding the ref reaches the same underlying element. Type `LabeledField`'s `ref` so the instance it exposes is derived from the `TextInput` component itself rather than written out by hand.
