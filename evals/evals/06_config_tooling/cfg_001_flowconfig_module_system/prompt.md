`main.js` imports a helper from `Discount.js` using a bare module name (`from 'Discount'`), and that file exists in the project, but `flow check` reports:

```
Cannot resolve module `Discount`. [cannot-resolve-module]
```

The code in both files is correct — the project is just not configured to resolve imports written as module names instead of relative paths. Update the project configuration so these imports resolve. Do not change the import to a relative path, and do not edit `main.js` or `Discount.js`.
