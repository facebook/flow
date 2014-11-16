---
id: arrays
title: Arrays
layout: docs
permalink: /docs/arrays.html
prev: variables.html
next: classes.html
---

Array types are simply instantiations of a special polymorphic Array class:
the type `Array<T>` describes arrays whose elements are of type `T`.

## Type Annotating Arrays

```javascript
/* @flow */
var a = [1, 2, 3];
var b: Array<number> = a.map(function (x) { return x + 1; })
```

In this code, we create an array with the literal `[1, 2, 3]`, and call a method map on it, getting another array whose type we annotate as `Array<number>`.

## Array Elements

Interestingly, the element type of an array is not fixed: it is a supertype of
the types of all elements written into the array. Just like other polymorphic
classes, array types are invariant in their element types.

For example:

```javascript
/* @flow */
var a = [];
for (var i = 0; i < 10; ++i) {
  if (i % 2 == 0) { a[i] = 0; }
  else { a[i] = ''; };
}

function foo(i): string { return a[i]; }
```

Running Flow produces the following error:

```
File "example.js", line 4, character 36:
number
is incompatible with
File "example.js", line 8, characters 17-22:
string
```

The type of a is not pinned to `Array<number>` by the element write `a[i] = 0`
at line 4: if it did, Flow would report an error for an incompatible element
write `a[i] = ''` at line 5. Instead, based on lines 4 and 5, the type of a
becomes `Array<T>` where `T` is `number` or `string`. Since it is impossible
to know which element is read on line 8, Flow must account for the possibility
that it could be `number`, in which case it would be incompatible with the
`string` annotation, as reported.

## Exporting Arrays

When an array is exported, its element type must be specified. This effectively "seals" the element type.

## TODO: Tuples
