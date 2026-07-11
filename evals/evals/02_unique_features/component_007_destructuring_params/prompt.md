Write Flow React components in `main.js` for a chart system that uses configuration objects as props.

`BarChart` takes two props:
- `data: ReadonlyArray<{label: string, value: number}>` — the data points
- `style` — an object with `barColor: string` and `barWidth: number`. Callers pass this as a single `style` prop, but the component should receive `barColor` and `barWidth` as separate variables directly in its parameters.

`BarChart` renders a `<div>`. Inside it, for each data point, render a `<div>` containing:
- A `<span>` with the label
- A `<div>` with `backgroundColor` set to `barColor` and `width` set to `barWidth * value` pixels, `height` 20 pixels. Use inline styles.

`ScatterPlot` takes two props:
- `points: ReadonlyArray<{x: number, y: number}>` — the data points
- `config` — an object with `dotRadius: number`, `dotColor: string`, and `scale: number`. Callers pass this as a single `config` prop, but the component should receive each field as a separate variable directly in its parameters.

`ScatterPlot` renders an `<svg>` with `width` and `height` of 400. For each point, render a `<circle>` with `cx` set to `x * scale`, `cy` set to `y * scale`, `r` set to `dotRadius`, and `fill` set to `dotColor`.

The code must pass `flow check` with zero errors.
