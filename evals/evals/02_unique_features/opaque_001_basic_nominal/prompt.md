Implement the money module in `main.js`. Another module, `checkout.js`, already imports it and uses its API — leave that file unchanged.

Money amounts are stored internally as a whole number of cents. Other modules that import this code must not be able to use a plain `number` where a money amount is expected, nor read a money amount back out as a plain `number` — to callers the amount type is distinct from `number`, even though internally it is just a count of cents.

Export the amount type and these functions:
- `fromDollars(dollars: number)` — build an amount from a dollar figure, rounding to the nearest cent
- `add(a, b)` — add two amounts
- `scale(amount, factor: number)` — multiply an amount by a factor, rounding to the nearest cent
- `format(amount)` — render the amount as a string like `$12.34`
