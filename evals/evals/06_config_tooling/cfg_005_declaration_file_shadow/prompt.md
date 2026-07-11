`main.js` imports `format` from `Money.js` and calls it with a number, which is the correct usage. But `Money.js` is generated code whose committed Flow types are wrong — they declare `format` as taking a `string` — so `flow check` fails:

```
Cannot call `format` with `1099` bound to `amount` because number is incompatible with string. [incompatible-type]
```

You must not edit `main.js`, and you must not edit the generated `Money.js` (a codegen step would overwrite any change). Provide the correct type for this module another way so the project type-checks, leaving the implementation file alone. `format` takes a `number` and returns a `string`.
