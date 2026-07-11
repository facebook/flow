Write a Flow component `DataTable` in `main.js` that renders a collection of items as an HTML table.

The component is generic over the item type. Each item must have at least an `id` (string) and a `label` (string), but may have additional properties.

Props:
- `items`: an array of items
- `renderRow`: a callback that receives an item and returns the content for that row
- `sortBy` (optional): either `'id'` or `'label'`, defaults to `'id'` — items are sorted by this field before rendering

Render a `<table>` with:
- A `<thead>` containing a row with headers "ID" and "Content"
- A `<tbody>` where each item becomes a `<tr>` (keyed by the item's `id`) with two `<td>` cells: the item's `id` and the output of `renderRow`

The code must pass `flow check` with zero errors.
