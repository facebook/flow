Write Flow code for a storage quota system.

Quotas are measured in bytes and can exceed JavaScript's safe integer range, so they must be tracked as big integers.

Define an enum `StorageTier` with three members and these exact byte quotas:
- `Standard` = 1099511627776 (1 TiB)
- `Premium` = 1125899906842624 (1 PiB)
- `Unlimited` = 1152921504606846976 (1 EiB)

Write:
- `parseTier(input: bigint): StorageTier` — convert a raw quota value to the tier, defaulting to `Standard` when the value does not match a known tier
- `quotaBytes(tier: StorageTier): bigint` — return the underlying byte quota for the tier
- `allowsArchival(tier: StorageTier): boolean` — return `true` only for `Premium` and `Unlimited`, `false` otherwise
