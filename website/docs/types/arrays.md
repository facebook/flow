---
title: Array Types
slug: /types/arrays
---

> **Note:** Arrays are also sometimes used as tuples in JavaScript, these are
> annotated differently in Flow. See the Tuple docs for more information.

Arrays are a special list-like type of object in JavaScript. You can create
arrays a couple different ways.

```js
new Array(1, 2, 3); // [1, 2, 3];
new Array(3);       // [undefined, undefined, undefined]
[1, 2, 3];          // [1, 2, 3];
```

You can also create arrays and add values to them later on:

```js
let arr = []; // []
arr[0] = 1;   // [1]
arr[1] = 2;   // [1, 2]
arr[2] = 3;   // [1, 2, 3]
```

## `Array` Type {#toc-array-type}

To create an array type you can use `Array<Type>` type where `Type` is the type
of elements in the array. For example, to create a type for an array of numbers
you use `Array<number>`.

```js
let arr: Array<number> = [1, 2, 3];
```

You can put any type within `Array<Type>`.

```js
let arr1: Array<boolean> = [true, false, true];
let arr2: Array<string> = ["A", "B", "C"];
let arr3: Array<mixed> = [1, true, "three"]
```

## `Array` Type Shorthand Syntax {#toc-array-type-shorthand-syntax}

There's also a slightly shorter form of this syntax: `Type[]`.

```js flow-check
let arr: number[] = [0, 1, 2, 3];
```

Just note that `?Type[]` is the equivalent of `?Array<T>` and not `Array<?T>`.

```js flow-check
// @flow
let arr1: ?number[] = null;   // Works!
let arr2: ?number[] = [1, 2]; // Works!
let arr3: ?number[] = [null]; // Error!
```

If you want to make it `Array<?T>` you can use parenthesis like: `(?Type)[]`

```js flow-check
// @flow
let arr1: (?number)[] = null;   // Error!
let arr2: (?number)[] = [1, 2]; // Works!
let arr3: (?number)[] = [null]; // Works!
```

## Array access is unsafe {#toc-array-access-is-unsafe}

When you retrieve an element from an array there is always a possibility that
it is `undefined`. You could have either accessed an index which is out of the
bounds of the array, or the element could not exist because it is a "sparse
array".

For example, you could be accessing an element that is out of the bounds of the
array.

```js flow-check
// @flow
let array: Array<number> = [0, 1, 2];
let value: number = array[3]; // Works.
                       // ^ undefined
```

Or you could be accessing an element that does not exist if it is a "sparse
array".

```js flow-check
// @flow
let array: Array<number> = [];

array[0] = 0;
array[2] = 2;

let value: number = array[1]; // Works.
                       // ^ undefined
```

In order to make this safe, Flow would have to mark every single array access
as "*possibly undefined"*.

Flow does not do this because it would be extremely inconvenient to use. You
would be forced to refine the type of every value you get when accessing an
array.

```js flow-check
let array: Array<number> = [0, 1, 2];
let value: number | void = array[1];

if (value !== undefined) {
  // number
}
```

As Flow is made to be smarter it may be possible in the future to fix this
problem, but for now you should be aware of it.

## `$ReadOnlyArray<T>` {#toc-readonlyarray}

Similar to [`$ReadOnly<T>`](../utilities/#toc-readonly), it is the supertype
of all arrays and all tuples and represents a read-only view of an array. It does
not contain any methods that will allow an object of this type to be mutated
(no `push()`, `pop()`, etc.).

```js flow-check
// @flow
const readonlyArray: $ReadOnlyArray<number> = [1, 2, 3]

const first = readonlyArray[0] // OK to read
readonlyArray[1] = 20          // Error!
readonlyArray.push(4)          // Error!
```

Note that an array of type `$ReadOnlyArray<T>` can still have mutable _elements_:

```js flow-check
// @flow
const readonlyArray: $ReadOnlyArray<{x: number}> = [{x: 1}];
readonlyArray[0] = {x: 42}; // Error!
readonlyArray[0].x = 42; // OK
```

The main advantage to using `$ReadOnlyArray` instead of `Array` is that `$ReadOnlyArray`'s
type parameter is *covariant* while `Array`'s type parameter is *invariant*. That means that
`$ReadOnlyArray<number>` is a subtype of `$ReadOnlyArray<number | string>` while
`Array<number>` is NOT a subtype of `Array<number | string>`. So it's often useful to use
`$ReadOnlyArray` in type annotations for arrays of various types of elements.
Take, for instance, the following scenario:

```js flow-check
// @flow
const someOperation = (arr: Array<number | string>) => {
  // Here we could do `arr.push('a string')`
}

const array: Array<number> = [1]
someOperation(array) // Error!
```

Since the parameter `arr` of the `someOperation` function is typed as a mutable
`Array`, pushing a string into it would be possible inside that scope, which
would then break the type contract of the outside `array` variable. By
annotating the parameter as `$ReadOnlyArray` instead in this case, Flow can be
sure this won't happen and no errors will occur:

```js flow-check
// @flow
const someOperation = (arr: $ReadOnlyArray<number | string>) => {
  // Nothing can be added to `arr`
}

const array: Array<number> = [1]
someOperation(array) // Works!
```
