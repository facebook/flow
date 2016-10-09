---
id: arrays
title: Arrays
permalink: /docs/arrays.html
prev: syntax.html
next: classes.html
---

Array types are simply instantiations of a special polymorphic Array class:
the type `Array<T>` describes arrays whose elements are of type `T`.  The
shorthand syntax `T[]` is equivalent to `Array<T>`.

## Type Annotating Arrays

```js +line_numbers
/* @flow */
var a = [1, 2, 3];
var b: Array<number> = a.map(function(x) { return x + 1; });
```

In this code, we create an array with the literal `[1, 2, 3]`, and call a method map on it, getting another array whose type we annotate as `Array<number>`.

## Array Elements

Interestingly, the element type of an array is not fixed: it is a supertype of
the types of all elements written into the array. Just like other polymorphic
classes, array types are invariant in their element types.

For example:

```js +line_numbers
/* @flow */
var a = [];
for (var i = 0; i < 10; ++i) {
  if (i % 2 == 0) {
    a[i] = 0;
  } else {
    a[i] = '';
  };
}

function foo(i): string { return a[i]; }
```

Running Flow produces the following error:

```text
example.js:5:14,14: number
This type is incompatible with
 example.js:11:18,23: string
```
{: .cli-error}

The type of `a` is not pinned to `Array<number>` by the element write `a[i] = 0`
at line 4: if it did, Flow would report an error for an incompatible element
write `a[i] = ''` at line 5. Instead, based on lines 4 and 5, the type of `a`
becomes `Array<T>` where `T` is `number` or `string`. Since it is impossible
to know which element is read on line 11, Flow must account for the possibility
that it could be `number`, in which case it would be incompatible with the
`string` annotation, as reported.

## Exporting Arrays

When an array is exported, its element type must be specified. This effectively "seals" the element type.

## Tuples

Given types `T0`, `T1`, ..., `Tn`, a tuple type `[T0, T1, ..., Tn]` is an
array. That array has a general type `Array<T0 | T1 | ... | Tn>`, but Flow also
understands that accessing the element at index `n` will evaluate to the
specific type `Tn`.

Note that a tuple type of length `n` doesn't guarantee inhabitant arrays never
grow beyond length `n`. Thus, `var xs: [number, string] = [0, "", 0]` is a
valid assignment. Flow will ensure that the values at indices < `n` are of the
correct specific type, but values >= `n` can still exist as long as they are
consistent with the general element type of the array.

### Syntax

Tuples are arrays, so they are declared like arrays

```js +line_numbers
[<type1>, <type2>, <type3>, ...]
```

The elements of a tuple are accessed by their indices, where the exact type
for that particular index will be returned.

### Example

```js +line_numbers
/* @flow */
var tup = ["1", 1, true, "positive"];
var b = tup[1] * tup[3];
```

```
/tmp/flow/tup.js:2:26,35: string
This type is incompatible with
  /tmp/flow/tup.js:3:9,23: number

Found 1 errors
```

We declared a tuple with four (4) elements and tried to multiply a `number` with a `string`, and Flow caught it.
