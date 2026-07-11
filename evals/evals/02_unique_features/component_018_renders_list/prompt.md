Write Flow React components in `main.js` for a tab bar.

`Tab` takes `label: string` and renders an `<li>` with the label.

`TabBar` accepts any number of children — zero or more — and renders them inside a `<ul>`. Only `Tab` elements, or elements from components that produce `Tab` output, may be passed as children. The children may be provided individually or as arrays.

`CountTab` takes `label: string` and `count: number`. It delegates to `Tab` with a label of the form `label (count)` — for example, `Inbox (5)`. A `CountTab` element must be accepted by `TabBar`.

Write an `App` that renders a `TabBar` containing a mix of individual `Tab`/`CountTab` children and an array of `Tab` children.

The code must pass `flow check` with zero errors.
