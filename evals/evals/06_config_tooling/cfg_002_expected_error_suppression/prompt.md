`main.js` defines a `clamp` helper followed by a few type-level checks that document its contract. The last line — `clamp('5', 0, 10)` — is **intentionally invalid**: it exists to prove that Flow rejects a string argument, so the contract stays enforced if someone later loosens the signature.

Right now `flow check` fails on that line:

```
Cannot call `clamp` with `'5'` bound to `value` because string is incompatible with number. [incompatible-type]
```

Make `flow check` pass **while keeping that intentionally-invalid call in place** (do not delete it, change its arguments, or weaken the function's type). The intent is for that one line's error to be silenced as expected, not fixed.
