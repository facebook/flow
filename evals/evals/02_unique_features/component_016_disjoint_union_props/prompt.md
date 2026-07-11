Write a Flow React component in `main.js` for rendering lines in a shopping receipt.

`CartLine` accepts the props for exactly one of two kinds of line, distinguished by a `kind` field:
- A product line has `kind: 'product'`, `name: string`, `qty: number`, and `price: number`. It renders a `<div>` showing `name × qty = $total`, where `total` is `qty * price` formatted to 2 decimal places.
- A discount line has `kind: 'discount'`, `label: string`, and `amount: number`. It renders a `<div>` showing `label: −$amount`, with `amount` formatted to 2 decimal places.

A product line never carries the discount fields, and a discount line never carries the product fields.

Also write a `Receipt` component that renders a `<div>` containing at least one product line and one discount line.

The code must pass `flow check` with zero errors.
