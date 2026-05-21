// `satisfies` is gated; without `experimental.ts_syntax` or
// `experimental.tslib_syntax`, it should be rejected.
1 satisfies number; // ERROR: `satisfies` expression is not enabled
