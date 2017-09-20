---
layout: guide
---

Flow supports promises with the syntax of resolve value type.

```js
// @flow

async function demo(): Promise<string> {
    return 'I love cats!';
}

```

The reject case is not specified, because it is not a return type, it is an exception.

## Related Functions

#### all<Elem, T:Iterable<Elem>>(promises: T): Promise<$TupleMap<T, typeof $await>>;

#### race<T, Elem: Promise<T> | T>(promises: Array<Elem>): Promise<T>;
