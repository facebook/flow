// `experimental.ts_syntax=true` implicitly enables `experimental.tslib_syntax`,
// so `satisfies` works here without setting the latter explicitly.
1 satisfies number; // OK — feature is enabled (no `unsupported-syntax` error)
"hello" satisfies number; // ERROR: string is not a number — the subtype check still runs
