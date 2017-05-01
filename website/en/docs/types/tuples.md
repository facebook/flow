---
layout: guide
---

Tuples are a sort of list but with a limited set of items. In JavaScript,
tuples are created using arrays.

In Flow you can create tuples using the `[type, type, type]` syntax.

```js
let tuple1: [number] = [1];
let tuple2: [number, boolean] = [1, true];
let tuple3: [number, boolean, string] = [1, true, "three"];
```

When you are getting a value from a tuple at a specific index, it will return the
type at that index.

```js
// @flow
let tuple: [number, boolean, string] = [1, true, "three"];

let num  : number  = tuple[0]; // Works!
let bool : boolean = tuple[1]; // Works!
let str  : string  = tuple[2]; // Works!
```

If you try getting from an index that does not exist it will return a type of
`void`.

```js
// @flow
let tuple: [number, boolean, string] = [1, true, "three"];

let none: void = tuple[3];
```

If Flow doesn't know which index you are trying to access it will return all
possible types.

```js
// @flow
let tuple: [number, boolean, string] = [1, true, "three"];

function getItem(n: number) {
  let val: number | boolean | string = tuple[n];
  // ...
}
```

When setting a new value inside a tuple, the new value must match the type at
that index.

```js
// @flow
let tuple: [number, boolean, string] = [1, true, "three"];

tuple[0] = 2;     // Works!
tuple[1] = false; // Works!
tuple[2] = "foo"; // Works!

// $ExpectError
tuple[0] = "bar"; // Error!
// $ExpectError
tuple[1] = 42;    // Error!
// $ExpectError
tuple[2] = false; // Error!
```

## Strictly enforced tuple length (arity) <a class="toc" id="toc-strictly-enforced-tuple-length-arity" href="#toc-strictly-enforced-tuple-length-arity"></a>

The length of the tuple is known as the "arity". The length of a tuple is
strictly enforced in Flow.

##### Tuples only match tuples with same length <a class="toc" id="toc-tuples-only-match-tuples-with-same-length" href="#toc-tuples-only-match-tuples-with-same-length"></a>

This means that a shorter tuple can't be used in place of a longer one.

```js
// @flow
let tuple1: [number, boolean]       = [1, true];
// $ExpectError
let tuple2: [number, boolean, void] = tuple1; // Error!
```

Also, a longer tuple can't be used in place of a shorter one.

```js
// @flow
let tuple1: [number, boolean, void] = [1, true];
// $ExpectError
let tuple2: [number, boolean]       = tuple1; // Error!
```

##### Tuples don't match array types <a class="toc" id="toc-tuples-don-t-match-array-types" href="#toc-tuples-don-t-match-array-types"></a>

Since Flow does not know the length of an array, an `Array<T>` type cannot be
passed into a tuple.

```js
// @flow
let array: Array<number>    = [1, 2];
// $ExpectError
let tuple: [number, number] = array; // Error!
```

Also a tuple type cannot be passed into to an `Array<T>` type, since then you
could mutate the tuple in an unsafe way.

```js
// @flow
let tuple: [number, number] = [1, 2];
// $ExpectError
let array: Array<number>    = tuple; // Error!
```

##### Cannot use mutating array methods on tuples <a class="toc" id="toc-cannot-use-mutating-array-methods-on-tuples" href="#toc-cannot-use-mutating-array-methods-on-tuples"></a>

You cannot use `Array.prototype` methods that mutate the tuple, only ones that
do not.

```js
// @flow
let tuple: [number, number] = [1, 2];
tuple.join(', '); // Works!
// $ExpectError
tuple.push(3);    // Error!
```
