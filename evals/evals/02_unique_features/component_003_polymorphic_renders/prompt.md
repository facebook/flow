Write Flow components for a dashboard widget system in `main.js`.

`Widget` takes `title: string` and `value: number`. It renders a `<div>` containing an `<h3>` with the title and a `<span>` with the value formatted to 2 decimal places.

`PercentWidget` takes `title: string` and `ratio: number` (between 0 and 1). It computes the percentage (`ratio * 100`), clamps it to the range 0–100, and delegates to `Widget` with the computed percentage as the value. It should be usable wherever a `Widget` element is expected.

`DeltaWidget` takes `title: string`, `current: number`, and `previous: number`. It computes the change (`current - previous`) and delegates to `Widget` with the change as the value. If `previous` is zero, it uses `current` as the value instead. It should also be usable wherever a `Widget` element is expected.

`Dashboard` takes `title: string` and children that must be `Widget` elements (or elements from components that produce `Widget` output). It renders a `<section>` containing an `<h2>` with the title and the children.

Write an `App` with a `Dashboard` containing all three widget types.

The code must pass `flow check` with zero errors.
