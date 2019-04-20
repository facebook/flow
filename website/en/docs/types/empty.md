---
layout: guide
---

Empty type has no values, it is also known as [bottom type](https://en.wikipedia.org/wiki/Bottom_type).

The `empty` type is a subtype of _every_ type.

For example, `empty` type is the return type of a function that throws exception:

```js
// @flow
function error(message: string): empty {
  throw new Error(message)
}
```

or never returns:

```js
// @flow
function loop(): empty {
  while (true) {

  }
}
```

Functions that return `empty` are called _diverging functions_.