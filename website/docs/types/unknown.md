---
title: Unknown
slug: /types/unknown
---

import {SinceVersion} from '../../components/VersionTags';

`unknown` (replacing [`mixed`](mixed.md) since <SinceVersion version="0.290" />) is the [supertype of all types](../../lang/type-hierarchy). All values are `unknown`.
However, this means that very few operations are permitted on it, without refining to some more specific type.
That's because the valid operations on `unknown` must be valid for all types.

In general, programs have several different categories of types:

**A single type:**

Here the input value can only be a `number`.

```js flow-check
function square(n: number) {
  return n * n;
}
```

**A group of different possible types:**

Here the input value could be either a `string` or a `number`.

```js flow-check
function stringifyBasicValue(value: string | number) {
  return '' + value;
}
```

**A type based on another type:**

Here the return type will be the same as the type of whatever value is passed
into the function.

```js flow-check
function identity<T>(value: T): T {
  return value;
}
```

These three are the most common categories of types. They will make up the
majority of the types you'll be writing.

However, there is also a fourth category.

**An arbitrary type that could be anything:**

Here the passed in value is an unknown type, it could be any type and the
function would still work.

```js flow-check
function getTypeOf(value: unknown): string {
  return typeof value;
}
```

These unknown types are less common, but are still useful at times.

You should represent these values with `unknown`.

## Anything goes in, Nothing comes out {#toc-anything-goes-in-nothing-comes-out}

`unknown` will accept any type of value. Strings, numbers, objects, functions–
anything will work.

```js flow-check
function stringify(value: unknown) {
  // ...
}

stringify("foo");
stringify(3.14);
stringify(null);
stringify({});
```

When you try to use a value of a `unknown` type you must first figure out what
the actual type is or you'll end up with an error.

```js flow-check
function stringify(value: unknown) {
  return "" + value; // Error!
}

stringify("foo");
```

Instead you must ensure the value is a certain type by [refining](../../lang/refinements/) it.

```js flow-check
function stringify(value: unknown) {
  if (typeof value === 'string') {
    return "" + value; // Works!
  } else {
    return "";
  }
}

stringify("foo");
```

Because of the `typeof value === 'string'` check, Flow knows the `value` can
only be a `string` inside of the `if` statement. This is known as a
[refinement](../../lang/refinements/).

## Versus `any`
`unknown` is safe, while [`any`](../any) is not. Both accept all values, but `any` also unsafely allows all operations.

## Versus `empty`
`unknown` is the opposite of [`empty`](../empty):
- Everything is a `unknown`, but few operations are permitted on it without first refining to a specific type. It is the supertype of all types.
- Nothing is `empty`, but any operation is permitted on it. It is the subtype of all types.
