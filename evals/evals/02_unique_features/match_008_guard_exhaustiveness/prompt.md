`main.js` defines an `Item` type with a `name`, a `price`, and a `category` of `'food'`, `'electronics'`, or `'clothing'`.

Write a Flow function `discount(item: Item): number` that uses a `match` expression to apply category-specific discounts based on price thresholds.

- `'food'` items over $50 get a 10% discount
- `'electronics'` items over $100 get a 15% discount
- `'clothing'` items over $75 get a 20% discount
- All other items return their original price
