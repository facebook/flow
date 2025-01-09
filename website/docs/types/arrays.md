---
title: Arrays
slug: /types/arrays
---

Array types represent lists of **unknown length**, where all items have the **same type**.
This is in contrast to [tuple types](../tuples), which have a fixed length and where each element can have a different type.

JavaScript array literal values can be used to create both tuple and array types:

```js flow-check
const arr: Array<number> = [1, 2, 3]; // As an array type
const tup: [number, number, number] = [1, 2, 3]; // As a tuple type
```

## `Array` Type {#toc-array-type}

The type `Array<T>` represents an array of items of type `T`.
For example, an array of numbers would be `Array<number>`:

```js flow-check
const arr: Array<number> = [1, 2, 3];
```

You can put any type within `Array<T>`:

```js flow-check
const arr1: Array<boolean> = [true, false, true];
const arr2: Array<string> = ["A", "B", "C"];
const arr3: Array<mixed> = [1, true, "three"];
```

## `$ReadOnlyArray` Type {#toc-readonlyarray}

You can use the type `$ReadOnlyArray<T>` instead of `Array<T>` to represent a [read-only](../../lang/variance) array which cannot be mutated.
You can't write to a read-only array directly, and can't use methods which mutate the array like `.push()`, `.unshift()`, etc.

```js flow-check
const readonlyArray: $ReadOnlyArray<number> = [1, 2, 3]

const first = readonlyArray[0]; // OK to read
readonlyArray[1] = 20;          // Error!
readonlyArray.push(4);          // Error!
readonlyArray.unshift(4);       // Error!
```

Note that an array of type `$ReadOnlyArray<T>` can still have mutable _elements_:

```js flow-check
const readonlyArray: $ReadOnlyArray<{x: number}> = [{x: 1}];
readonlyArray[0] = {x: 42}; // Error!
readonlyArray[0].x = 42; // Works
```

The main advantage to using `$ReadOnlyArray` instead of `Array` is that `$ReadOnlyArray`'s
type parameter is [*covariant*](../../lang/variance/#toc-covariance)
while `Array`'s type parameter is [*invariant*](../../lang/variance/#toc-invariance).
That means that `$ReadOnlyArray<number>` is a subtype of `$ReadOnlyArray<number | string>` while
`Array<number>` is NOT a subtype of `Array<number | string>`. So it's often useful to use
`$ReadOnlyArray` in type annotations for arrays of various types of elements.
Take, for instance, the following scenario:

```js flow-check
const someOperation = (arr: Array<number | string>) => {
  // Here we could do `arr.push('a string')`
}

const array: Array<number> = [1];
someOperation(array) // Error!
```

Since the parameter `arr` of the `someOperation` function is typed as a mutable
`Array`, pushing a string into it would be possible inside that scope, which
would then break the type contract of the outside `array` variable. By
annotating the parameter as `$ReadOnlyArray` instead in this case, Flow can be
sure this won't happen and no errors will occur:

```js flow-check
const someOperation = (arr: $ReadOnlyArray<number | string>) => {
  // Nothing can be added to `arr`
}

const array: Array<number> = [1];
someOperation(array); // Works
```

`$ReadOnlyArray<mixed>` represents the supertype of all arrays and all [tuples](../tuples):

```js flow-check
const tup: [number, string] = [1, 'hi'];
const arr: Array<number> = [1, 2];

function f(xs: $ReadOnlyArray<mixed>) { /* ... */ }

f(tup); // Works
f(arr); // Works
```

## Empty Array Literals {#toc-empty-array-literals}

Empty array literals (`[]`) are handled specially in Flow when it comes to their [annotation requirements](../../lang/annotation-requirement).
What makes them special is that they do not contain enough information to
determine their type, and at the same time they are too common to always
require type annotations in their immediate context.

So, to type empty arrays Flow follows these rules:

### Contextual Inference

First, if [contextual information](../../lang/annotation-requirement/#toc-contextual-typing) exists,
we'll use it to determine the array element type:

```js flow-check
function takesStringArray(x: Array<string>): void {}

const arr1: Array<string> = [];
takesStringArray([]);
```

In both cases, the `[]` will be typed as an `Array<string>`.

Note that for the contextual information to work, the type needs to be available
right at the definition of the array. This means that the last two lines in the following
code will error:

``` js flow-check
function takesStringArray(x: Array<string>): void {}

const arr2 = [];
takesStringArray(arr2);
```

The second error is due to `arr2` being inferred as `Array<empty>` which leads to another error at the call
to `takesStringArray`.

### Initializer Inference

Flow allows another way to determine the types of empty arrays when they are immediately assigned to a variable:

```js
const arr3 = [];
```

The way it does this is reminiscent of the typing of
[variables without initializers](../../lang/variables/#toc-variables-declared-without-initializers):
Flow tries to choose the "first" *assignment* or *assignments*
to the variable to define its type. In the case of empty arrays, an assignment is either
* an indexed write statement `a[i] = e;`, or
* an array `push` call `a.push(e)`.

In either case the type of `e` is used as the type of the array element.

Here are some examples:

#### Straight-line Code
Once the first assignment has been found, the type of the array element is pinned to that of the
assigned expression. Subsequent writes to array with elements of
a different type are errors:

``` js flow-check
const arr3 = [];
arr3.push(42); // arr3 is inferred as Array<number>
arr3.push("abc"); // Error!
```

#### Conditional Code

If the array is assigned in sibling branches of conditional statements, the type
of the array element is pinned to the union of the assigned types:

``` js flow-check
declare const cond: boolean;

const arr4 = [];
if (cond) {
  arr4[0] = 42;
} else {
  arr4.push("abc");
}
// arr4 is inferred as Array<number | string>
arr4.push("def"); // Works
arr4[0] = true; // Error!
```

#### Nearer Scope Wins

Shallow scope of assignment is prefered when there are multiple scopes where assignments happen:

``` js flow-check
const arr5 = [];
function f() {
  arr5.push(42); // Error!
}
f();
arr5.push("abc"); // This assignment wins. arr5 is inferred as Array<string>
arr5.push(1); // Error!
```

## Array access is unsafe {#toc-array-access-is-unsafe}

When you retrieve an element from an array there is always a possibility that
it is `undefined`. You could have either accessed an index which is out of the
bounds of the array, or the element could not exist because it is a "sparse
array".

For example, you could be accessing an element that is out of the bounds of the
array:

```js flow-check
const array: Array<number> = [0, 1, 2];
const value: number = array[3]; // Works
                         // ^ undefined
```

Or you could be accessing an element that does not exist if it is a "sparse array":

```js flow-check
const array: Array<number> = [];

array[0] = 0;
array[2] = 2;

const value: number = array[1]; // Works
                         // ^ undefined
```

In order to make this safe, Flow would have to mark every single array access
as "*possibly undefined"*.

Flow does not do this because it would be extremely inconvenient to use. You
would be forced to refine the type of every value you get when accessing an
array.

```js flow-check
const array: Array<number> = [0, 1, 2];
const value: number | void = array[1];

if (value !== undefined) {
  // number
}
```

## Discouraged Array Type Shorthand Syntax {#toc-array-type-shorthand-syntax}

There is an alternative to the `Array<T>` syntax: `T[]`.
This syntax is discouraged and may be deprecated in the future.

```js flow-check
const arr: number[] = [0, 1, 2, 3];
```

Just note that `?Type[]` is the equivalent of `?Array<T>` and not `Array<?T>`.

```js flow-check
const arr1: ?number[] = null;   // Works
const arr2: ?number[] = [1, 2]; // Works
const arr3: ?number[] = [null]; // Error!
```

If you want to make it `Array<?T>` you can use parenthesis like: `(?Type)[]`

```js flow-check
const arr1: (?number)[] = null;   // Error!
const arr2: (?number)[] = [1, 2]; // Works
const arr3: (?number)[] = [null]; // Works
```
