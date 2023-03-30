---
title: Tuple Types
slug: /types/tuples
---

Tuples are a fixed length list, where each element can have a different type.
This is in contrast to [arrays](../arrays), which have an undetermined length, and all elements have the same type.

JavaScript array literal values can be used to create both tuple and array types.

In Flow you can create tuple types using the `[type, type, type]` syntax:

```js flow-check
const tuple1: [number] = [1];
const tuple2: [number, boolean] = [1, true];
const tuple3: [number, boolean, string] = [1, true, "three"];
```

When you get a value from a tuple at a specific index, it will return the
type at that index:

```js flow-check
const tuple: [number, boolean, string] = [1, true, "three"];

const num: number = tuple[0]; // Works!
const bool: boolean = tuple[1]; // Works!
const str: string  = tuple[2]; // Works!
```

Trying to access an index that does not exist results in an index-out-of-bounds error:

```js flow-check
const tuple: [number, boolean, string] = [1, true, "three"];

const none = tuple[3]; // Error!
```

If Flow doesn't know which index you are trying to access it will return all
possible types:

```js flow-check
const tuple: [number, boolean, string] = [1, true, "three"];

function getItem(n: number) {
  const val: number | boolean | string = tuple[n];
  // ...
}
```

When setting a new value inside a tuple, the new value must match the type at
that index:

```js flow-check
const tuple: [number, boolean, string] = [1, true, "three"];

tuple[0] = 2;     // Works!
tuple[1] = false; // Works!
tuple[2] = "foo"; // Works!

tuple[0] = "bar"; // Error!
tuple[1] = 42;    // Error!
tuple[2] = false; // Error!
```

## Strictly enforced tuple length (arity) {#toc-strictly-enforced-tuple-length-arity}

The length of the tuple is known as the "arity". The length of a tuple is
strictly enforced in Flow.

This means that a shorter tuple can't be used in place of a longer one:

```js flow-check
const tuple1: [number, boolean] = [1, true];

const tuple2: [number, boolean, void] = tuple1; // Error!
```

Also, a longer tuple can't be used in place of a shorter one:

```js flow-check
const tuple1: [number, boolean, void] = [1, true, undefined];

const tuple2: [number, boolean] = tuple1; // Error!
```

## Tuples don't match array types {#toc-tuples-don-t-match-array-types}

Since Flow does not know the length of an array, an `Array<T>` type cannot be
passed into a tuple:

```js flow-check
const array: Array<number> = [1, 2];

const tuple: [number, number] = array; // Error!
```

Also a tuple type cannot be passed into to an `Array<T>` type, since then you
could mutate the tuple in an unsafe way (for example, `push`ing a third item onto it):

```js flow-check
const tuple: [number, number] = [1, 2];

const array: Array<number> = tuple; // Error!
```

However, you can pass it to a [`$ReadOnlyArray`](../arrays/#toc-readonlyarray) type, since mutation is disallowed:

```js flow-check
const tuple: [number, number] = [1, 2];

const array: $ReadOnlyArray<number> = tuple; // Works!
```

## Cannot use mutating array methods on tuples {#toc-cannot-use-mutating-array-methods-on-tuples}

You cannot use `Array.prototype` methods that mutate the tuple, only ones that do not:

```js flow-check
const tuple: [number, number] = [1, 2];
tuple.join(', '); // Works!

tuple.push(3); // Error!
```
