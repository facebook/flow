---
title: Literal Types
slug: /types/literals
---

Flow has [primitive types](../primitives) for
literal values, but can also use literal values as types.

For example, instead of accepting `number` type, we could accept only the
literal value `2`.

```js
// @flow
function acceptsTwo(value: 2) {
  // ...
}

acceptsTwo(2);   // Works!
// $ExpectError
acceptsTwo(3);   // Error!
// $ExpectError
acceptsTwo("2"); // Error!
```

You can use primitive values for these types:

- Booleans: like `true` or `false`
- Numbers: like `42` or `3.14`
- Strings: like `"foo"` or `"bar"`

Using these with [union types](../unions) is powerful:

```js flow-check
// @flow
function getColor(name: "success" | "warning" | "danger") {
  switch (name) {
    case "success" : return "green";
    case "warning" : return "yellow";
    case "danger"  : return "red";
  }
}

getColor("success"); // Works!
getColor("danger");  // Works!
// $ExpectError
getColor("error");   // Error!
```
