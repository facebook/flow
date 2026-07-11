The file `source.ts` contains a TypeScript React `Card` component that forwards a
ref to its root element and renders optional content regions.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior — the
component must still forward a ref to the root `<div>`. The result must pass
`flow check` with zero errors.
