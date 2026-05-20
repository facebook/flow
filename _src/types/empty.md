---
title: Empty
slug: /types/empty
description: "How the empty (bottom) type works in Flow, representing a type with no values that is a subtype of all other types."
---

The `empty` type has no values. It is the [subtype of all other types](../lang/type-hierarchy.md) (the [bottom type](https://en.wikipedia.org/wiki/Bottom_type)), the opposite of [`unknown`](./unknown.md).

```js flow-check
function throwIt(msg: string): empty {
  throw new Error(msg);
}
```

## When to use this {#toc-when-to-use}

Use `empty` as the return type of functions that never return (they always throw). You can also cast to `empty` to assert exhaustiveness — if you've refined away all members of a union, the remaining value should be `empty`:

```js flow-check
function f(x: 'a' | 'b'): number {
  switch (x) {
    case 'a':
      return 1;
    case 'b':
      return 2;
    default:
      return x as empty;
  }
}
```

If you had not checked for all members of the union (for example, changed `x` to be of type `'a' | 'b' | 'c'`),
then `x` would no longer be `empty` in the `default`, and Flow would error.

> Note: If you want exhaustively checked enums by default, without having to cast to `empty`,
> you could enable and use [Flow Enums](../enums/index.md) in your project.

Since `empty` is the subtype of all types, all operations are permitted on something that has the `empty` type.
However since no values can be `empty`, this is "safe", unlike with [`any`](./any.md).

```js flow-check
const str = "hello";

if (typeof str === "string") {
  str as string; // Yes it's a string
} else {
  // Works! Since we will never enter this branch
  str as empty;
  const n: number = str + 1;
}
```

We put "safe" in quotes above, as due type safety holes in your code or bugs within Flow itself,
it is possible to get values which are `empty` typed.

You can use the [coverage](../cli/coverage.md) command to identify code typed as `empty`.

## See Also {#toc-see-also}

- [Unknown](./unknown.md) — the supertype of all types (the opposite of `empty`)
- [Any](./any.md) — an unsafe escape hatch that is both the top and bottom type
- [Type Hierarchy](../lang/type-hierarchy.md) — how `empty`, `unknown`, and `any` relate in the full type hierarchy
- [Unions](./unions.md) — casting to `empty` in a `default` branch asserts exhaustiveness over union members
- [Match Expressions](../match/index.md) — pattern matching with built-in exhaustiveness checking
