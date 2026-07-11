Write a Flow function `applyDiscount(price: number, discount: Discount): number` that uses a `match` expression to compute the price after a discount is applied.

The `PercentOff`, `AmountOff`, and `BuyOneGetOne` classes are provided, and `Discount` is their union. Apply each kind of discount to `price`:

- `PercentOff`: reduce `price` by its `percent` (a `percent` of 25 means the customer pays 75% of `price`)
- `AmountOff`: subtract its `amount`, but never return less than `0`
- `BuyOneGetOne`: the customer pays for half
