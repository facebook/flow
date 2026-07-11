Write Flow code in `main.js` for a feature-flagged hook system.

Write two hooks:
- `useLocalCounter` takes `initial: number` and returns `[number, () => void]` — the count and an increment function that adds 1 each time.
- `useAlwaysZero` takes `initial: number` (ignored) and returns `[number, () => void]` — always returns 0 and a no-op function.

Both hooks must have the same type signature.

Declare a variable `useActiveCounter` with an explicit type annotation, and assign it conditionally: if the global `__DEV__` is true, use `useLocalCounter`; otherwise use `useAlwaysZero`.

Write a component `Counter` that calls `useActiveCounter(0)`, destructures the count and increment function, and renders a `<div>` with:
- A `<span>` showing the count
- A `<button>` that calls the increment function when clicked, labeled "Increment"

The code must pass `flow check` with zero errors.
