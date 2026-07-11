This project sets `sketchy-null-string` to `error` in its `.flowconfig`, so `main.js` fails `flow check`:

```
Sketchy null check on string which is potentially an empty string. [sketchy-null-string]
```

The `if (!name)` check is **intentional**: an empty string should be treated as "no name" just like `null`/`undefined`, so the runtime behavior must not change. Make `flow check` pass by silencing this lint for only this one occurrence. Do not edit the `.flowconfig` (the rule should stay on everywhere else), do not change the runtime behavior, and do not use an error-suppression comment.
