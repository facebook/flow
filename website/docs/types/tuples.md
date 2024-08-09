---
title: Tuples
slug: /types/tuples
---

Tuple types represent a **fixed length** list, where the elements can have **different types**.
This is in contrast to [array types](../arrays), which have an unknown length and all elements have the same type.

## Tuple Basics

JavaScript array literal values can be used to create both tuple and array types:

```js flow-check
const arr: Array<number> = [1, 2, 3]; // As an array type
const tup: [number, number, number] = [1, 2, 3]; // As a tuple type
```

In Flow you can create tuple types using the `[type1, type2, type3]` syntax:

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

[Optional elements](#optional-tuple-elements) make the arity into a range.

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

## Length refinement

You can refine a [union](../unions) of tuples by their length:

```js flow-check
type Union = [number, string] | [boolean];
function f(x: Union) {
  if (x.length === 2) {
    // `x` is `[number, string]`
    const n: number = x[0]; // OK
    const s: string = x[1]; // OK
  } else {
    // `x` is `[boolean]`
    const b: boolean = x[0];
  }
}
```

## Tuple element labels

> NOTE: This and the following sections require your tooling to be updated as described in the "Adoption" section at the end of this page.

You can add a label to tuple elements. This label does not affect the type of the tuple element,
but is useful in self-documenting the purpose of the tuple elements, especially when multiple elements have the same type.

```js flow-check
type Range = [x: number, y: number];
```

The label is also necessary to add a variance annotation or optionality modifier to an element (as without the label we would have parsing ambiguities).

## Variance annotations and read-only tuples

You can add [variance](../../lang/variance)  annotations (to denote read-only/write-only) on labeled tuple elements, just like on object properties:

```js flow-check
type T = [+foo: number, -bar: string];
```

This allows you to mark elements as read-only or write-only. For example:

```js flow-check
function f(readOnlyTuple: [+foo: number, +bar: string]) {
  const n: number = readOnlyTuple[0]; // OK to read
  readOnlyTuple[1] = 1; // ERROR! Cannot write
}
```

You can also use the [`$ReadOnly`](../utilities/#toc-readonly)  on tuple types as a shorthand for marking each property as read-only:

```js flow-check
type T = $ReadOnly<[number, string]>; // Same as `[+a: number, +b: string]`
```

## Optional tuple elements

You can mark tuple elements as optional with `?` after an element’s label. This allows you to omit the optional elements.
Optional elements must be at the end of the tuple type, after all required elements.

```js flow-check
type T = [foo: number, bar?: string];
[1, "s"] as T; // OK: has all elements
[1] as T; // OK: skipping optional element
```

You cannot write `undefined` to the optional element - add `| void` to the element type if you want to do so:

```js flow-check
type T = [foo?: number, bar?: number | void];
declare const x: T;
x[0] = undefined; // ERROR
[undefined] as T; // ERROR

x[1] = undefined; // OK: we've added `| void` to the element type
```

You can also use the [`Partial`](../utilities/#toc-partial) and [`Required`](../utilities/#toc-required) utility types to make all elements optional or required respectively:

```js flow-check
type AllRequired = [number, string];
[] as Partial<AllRequired>; // OK: like `[a?: number, b?: string]` now

type AllOptional = [a?: number, b?: string];
[] as Required<AllOptional>; // ERROR: like `[a: number, b: string]` now
```

Tuples with optional elements have an arity (length) that is a range rather than a single number. For example, `[number, b?: string]` has an length of 1-2.

## Tuple spread

You can spread a tuple type into another tuple type to make a longer tuple type:

```js flow-check
type A = [number, string];
type T = [...A, boolean]; // Same as `[number, string, boolean]`
[1, "s", true] as T; // OK
```

Tuple spreads preserve labels, variance, and optionality. You cannot spread arrays into tuples, only other tuples.

At the value level, if you spread a tuple with optional elements into an array literal, then you cannot have anything after that spread and retain the tuple view of the array value.
That is because a tuple with optional elements has a length that's a range, so we don't know at what index any subsequent values would be at.
You can still type this value as the appropriate `Array<T>` type - only the tuple view of the value is affected.

```js flow-check
const a: [foo?: 1] = [];
const b = [0, ...a, 2]; // At runtime this is `[0, 2]`
b as [0, 1 | void, 2]; // ERROR
b as Array<number | void>; // OK

const c: [0, foo?: 1] = [0];
const d: [bar?: 2] = [2];
const e = [...c, ...d]; // At runtime this is `[0, 2]`
e as [0, foo?: 1, bar?: 2]; // ERROR
e as Array<number | void>; // OK
```

## Inexact tuples
Inexact tuple types work like [inexact objects](../objects#exact-and-inexact-object-types): they allow for unknown members at the end of the tuple.

```js flow-check
[] as [...]; // OK
[1] as [...]; // OK
[1] as [number, ...]; // OK
```

All tuples are subtypes of the inexact empty tuple `[...]`.

If you spread an inexact tuple, the result is also inexact.
You cannot define elements after the spread of an inexact tuple, since we wouldn't know at what index they should be.

```js flow-check
declare const x: [1, ...];
const y = [0, ...x];
y as [0, 1]; // ERROR: it's inexact
y as [0, 1, ...]; // OK

[...x, 2]; // ERROR: can't have element after inexact spread
```

Inexact tuples allow you to require that a generic is a tuple, e.g.

```js flow-check
function mapTupleArray<T: [...], R>(
  tuples: Array<T>, // An array of tuples
  f: (...T) => R, // Function args match the tuple's types
): Array<R> {
  return tuples.map(args => f(...args));
}
mapTupleArray(
  [[1, 'hi'], [3, 'bye']],
  (x: number, y: string) => y.length === x,
); // OK

declare const arrays: Array<Array<number>>;
mapTupleArray(arrays, (x: number, y: number) => x + y); // ERROR: array is not a tuple
```

## Adoption

To use labeled tuple elements (including optional elements and variance annotations on elements) and tuple spread elements,
you need to upgrade your infrastructure so that it supports the syntax:

- `flow` and `flow-parser`: 0.212
- `prettier`: 3
- `babel` with `babel-plugin-syntax-hermes-parser` (v0.15). See [our Babel guide](../../tools/babel/) for setup instructions.
- `eslint` with `hermes-eslint` (v0.15). See [our ESLint guide](../../tools/eslint/) for setup instructions.

To use inexact tuples, upgrade to:
- `flow` and `flow-parser`: 0.243
- `prettier`: 3.3
- `babel` with `babel-plugin-syntax-hermes-parser` (v0.23). See [our Babel guide](../../tools/babel/) for setup instructions.
- `eslint` with `hermes-eslint` (v0.23). See [our ESLint guide](../../tools/eslint/) for setup instructions.
