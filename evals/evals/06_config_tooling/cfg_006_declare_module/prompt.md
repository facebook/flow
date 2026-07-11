`main.js` imports `chunk` from the third-party package `list-utils`, which ships no Flow types, so `flow check` reports:

```
Cannot resolve module `list-utils`. [cannot-resolve-module]
```

Flow loads library definitions from the `flow-typed/` directory at the project root. Add a library definition at `flow-typed/list-utils.js` that declares the `list-utils` module so the import resolves and `chunk` is correctly typed. `chunk(items, size)` splits `items` into consecutive groups of `size` elements and returns an array of those groups — it should be generic in the element type. Do not change `main.js`.
