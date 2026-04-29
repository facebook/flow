---
title: Literal Types
slug: /types/literals
description: "How to use literal types in Flow to restrict values to specific strings, numbers, booleans, or bigints."
---

Literal types restrict a value to a specific [primitive](./primitives.md) — a particular string, number, boolean, or bigint.

```js flow-check
function acceptsTwo(value: 2) { /* ... */ }

acceptsTwo(2);   // Works!
acceptsTwo(3);   // Error!
```

Literal types support booleans (`true`, `false`), numbers (`42`, `3.14`), strings (`"foo"`), and bigints (`42n`). They are commonly combined with [union types](./unions.md):

```js flow-check
function getColor(name: "success" | "warning" | "danger") {
  switch (name) {
    case "success" : return "green";
    case "warning" : return "yellow";
    case "danger"  : return "red";
  }
}

getColor("success"); // Works!
getColor("danger");  // Works!

getColor("error");   // Error!
```

## When to use this {#toc-when-to-use}

Use literal types when you need to restrict a value to specific constants rather than a broad type like `string` or `number`. They are especially useful in [unions](./unions.md) to define a fixed set of allowed values (e.g. `"success" | "error"`). For a more structured alternative to unions of literals, consider [Flow Enums](../enums/index.md).

## See Also {#toc-see-also}

- [Primitive Types](./primitives.md) — the base types (`number`, `string`, etc.) that literals specialize
- [Unions](./unions.md) — commonly used with literals to define finite sets of values
- [Flow Enums](../enums/index.md) — a structured alternative to unions of literal types
