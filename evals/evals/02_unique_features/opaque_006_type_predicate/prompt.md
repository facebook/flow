You are building a metrics module split across two files: `Metric.js` and `main.js`.

In `Metric.js`:
- Define a type for counter metrics. Callers must not be able to read a counter's fields directly — they must go through the accessor functions below. Counter metrics are fully opaque outside this file.
- Define a plain exported object type for gauge metrics with fields: `kind: 'gauge'`, `name: string`, `value: number`, `unit: string`.
- `type Metric` — the union of counter and gauge metrics.
- `makeCounter(name: string, value: number)` — build a counter metric.
- `isCounter(metric: Metric): boolean` — return `true` if the metric is a counter, `false` if it is a gauge. The return type should allow Flow to distinguish the two cases when used in a conditional.
- `counterName(metric)` — return the name of a counter metric.
- `counterValue(metric)` — return the numeric value of a counter metric.

In `main.js`:
- `formatMetric(metric: Metric): string` — for a counter return `"counter:<name>=<value>"`; for a gauge return `"gauge:<name>=<value><unit>"`.
- `sumCounters(metrics: ReadonlyArray<Metric>): number` — return the sum of the values of all counter metrics in the array.
