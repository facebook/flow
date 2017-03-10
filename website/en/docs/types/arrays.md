---
layout: guide
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

## `Array` Type <a class="toc" id="toc-array-type" href="#toc-array-type"></a>

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

## `Array` Type Shorthand Syntax <a class="toc" id="toc-array-type-shorthand-syntax" href="#toc-array-type-shorthand-syntax"></a>

There's also a slightly shorter form of this syntax: `Type[]`.

```js
let arr: number[] = [0, 1, 2, 3];
```

Just note that `?Type[]` is the equivalent of `?Array<T>` and not `Array<?T>`.

```js
let arr1: ?number[] = null;   // Works!
let arr2: ?number[] = [1, 2]; // Works!
// $ExpectError
let arr3: ?number[] = [null]; // Error!
```

If you want to make it `Array<?T>` you can use parenthesis like: `(?Type)[]`

```js
// $ExpectError
let arr1: (?number)[] = null;   // Error!
let arr2: (?number)[] = [1, 2]; // Works!
let arr3: (?number)[] = [null]; // Works!
```

## Array access is unsafe <a class="toc" id="toc-array-access-is-unsafe" href="#toc-array-access-is-unsafe"></a>

When you retrieve an element from an array there is always a possibility that
it `undefined`. You could have either accessed an index which is out of the
bounds of the array, or the element could not exist because it is a "sparse
array".

For example, you could be accessing an element that is out of the bounds of the
array.

```js
// @flow
let array: Array<number> = [0, 1, 2];
let value: number = array[3]; // Works.
                       // ^ undefined
```

Or you could be accessing an element that does not exist if it is a "sparse
array".

```js
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

```js
let array: Array<number> = [0, 1, 2];
let value: number | void = array[1];

if (value !== undefined) {
  // number
}
```

As Flow is made to be smarter it may be possible in the future to fix this
problem, but for now you should be aware of it.
