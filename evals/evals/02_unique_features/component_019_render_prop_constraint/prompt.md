Write Flow React components in `main.js` for a page layout system.

`Header` takes `text: string` and renders an `<h1>` with the text.

`Layout` takes two parameters: `header` and `body: string`. The `header` parameter is a dedicated slot that must be filled with a `Header` element, or an element from a component that produces `Header` output — any other kind of element must be rejected by the type checker. `Layout` renders a `<div>` containing the header slot followed by a `<main>` with the body text.

`SectionHeader` takes `section: string` and delegates to `Header`, passing the section name as the text. A `SectionHeader` element must be accepted by `Layout`'s header slot.

Write an `App` that renders a `Layout`, filling the header slot with a `SectionHeader`.

The code must pass `flow check` with zero errors.
