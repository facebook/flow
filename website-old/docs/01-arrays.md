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

Tuples types represent fixed length JavaScript arrays with known types for each
element. So while the type `Array<number>` represents a JavaScript array with unknown
length where each element has the type `number`, the type `[number, string]` represents
a JavaScript array of length 2 where the first element is a `number` and the
second element is a `string`.

More generally, given types `T0`, `T1`, ..., `Tn`, the tuple type
`[T0, T1, ..., Tn]` represents an array of length `n + 1`, where the element at
index `k` has the type `Tk`, assuming `0 <= k <= n`.

### Syntax

Tuples are arrays, so they are declared like array literals

```js +line_numbers
[<type1>, <type2>, <type3>, ...]
```

### Example

```js +line_numbers
/* @flow */
function mult_first_and_third(tup: [string, number, boolean, number]): number {
  return tup[1] * tup[3];
}
mult_first_and_third(["1", 1, true, "positive"])
```

```
5: mult_first_and_third(["1", 1, true, "positive"])
                                       ^ string. This type is incompatible with
2: function mult_first_and_third(tup: [string, number, boolean, number]): number {
                                                                ^ number

Found 1 errors
```
{: .cli-error}

### Arity

The arity (i.e. length) of a tuple is strictly enforced, so that Flow can be
relatively confident that a tuple of length `N` is actually an array with length
`N`. This has a few consequences.

An `Array<T>` type cannot flow to a tuple, since we don't know the array's
length

```js +line_numbers
function array_to_tuple(arr: Array<number>): [number, number] {
  return arr;
}
```

```text
2:   return arr;
            ^ array type. Only tuples and array literals with known elements can flow to
1: function array_to_tuple(arr: Array<number>): [number, number] {
                                                ^ tuple type

Found 1 errors
```
{: .cli-error}

You cannot use `Array.prototype` methods that mutate the tuple

```js +line_numbers
function mutate_tuple(tup: [number]): void {
  const str = tup.join(', '); // OK
  tup.push(123); // Error
}
```

```text
3:   tup.push(123); // Error
         ^ property `push`. Property not found in
3:   tup.push(123); // Error
     ^ $ReadOnlyArray

Found 1 errors
```
{: .cli-error}

A tuple type cannot flow to an `Array<T>` type, since then you could mutate the
tuple in an unsafe way.

```js +line_numbers
function tuple_to_array(tup: [number]): Array<number> {
  return tup;
}
```

```text
2:   return tup;
            ^ tuple type. This type is incompatible with the expected return type of
1: function tuple_to_array(tup: [number]): Array<number> {
                                           ^ array type

Found 1 errors
```
{: .cli-error}

A tuple type cannot flow to a longer tuple type.

```js +line_numbers
function short_to_long(tup: [number]): [number, void] {
  return tup;
}
```

```text
2:   return tup;
            ^ tuple type. Tuple arity mismatch. This tuple has 1 elements and cannot flow to the 2 elements of
1: function short_to_long(tup: [number]): [number, void] {
                                          ^ tuple type

Found 1 errors
```
{: .cli-error}

A tuple type cannot flow to a shorter tuple type.

```js +line_numbers
function long_to_short(tup: [number, number]): [number] {
  return tup;
}
```

```text
2:   return tup;
            ^ tuple type. Tuple arity mismatch. This tuple has 2 elements and cannot flow to the 1 elements of
1: function long_to_short(tup: [number, number]): [number] {
                                                  ^ tuple type

Found 1 errors
```
{: .cli-error}
