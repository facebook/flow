---
title: Literal Types
slug: /types/literals
---

Flow has [primitive types](../primitives) for
literal values, but can also use literal values as types.

For example, instead of accepting `number` type, we could accept only the
literal value `2`.

```js flow-check
function acceptsTwo(value: 2) { /* ... */ }

acceptsTwo(2);   // Works!

acceptsTwo(3);   // Error!
acceptsTwo("2"); // Error!
```

You can use primitive values for these types:

- Booleans: like `true` or `false`
- Numbers: like `42` or `3.14`
- Strings: like `"foo"` or `"bar"`
- BigInts: like `42n`

Using these with [union types](../unions) is powerful:

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

Consider using [Flow Enums](../../enums) instead of unions of literal types, if they fit your use-case.
