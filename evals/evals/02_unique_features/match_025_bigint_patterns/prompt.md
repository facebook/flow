Write a Flow function `accountLabel(id: bigint): string` that uses a `match` expression to label account IDs.

Account IDs are 64-bit integers. Three IDs are reserved:

- `0` is the `"system"` account
- `1` is the `"root"` account
- `2` is the `"service"` account

Any other ID returns `"account #<id>"` with the ID interpolated.
