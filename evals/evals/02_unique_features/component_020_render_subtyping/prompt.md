Write Flow React components in `main.js` for a badge system that demonstrates how a single produced element can satisfy slots with different cardinalities.

`Badge` takes `text: string` and renders a `<span>` with the text.

`StatusBadge` takes `ok: boolean` and delegates to `Badge`, passing `'OK'` when `ok` is true and `'FAIL'` otherwise. A `StatusBadge` element must be usable as a `Badge` element.

Write three slot components, each rendering its parameter inside a `<div>`:
- `RequiredSlot` requires exactly one `Badge` element.
- `OptionalSlot` accepts at most one `Badge` element, or nothing.
- `ManySlot` accepts any number of `Badge` elements (zero or more).

Write an `App` that creates one `StatusBadge` element and passes that same element into all three slots, demonstrating that the element produced for a single-element slot is also acceptable in the optional and the zero-or-more slots.

The code must pass `flow check` with zero errors.
