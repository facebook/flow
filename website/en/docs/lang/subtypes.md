---
layout: guide
---

## What is a subtype? <a class="toc" id="toc-what-is-a-subtype" href="#toc-what-is-a-subtype"></a>

A type like `number`, `boolean`, or `string` describes a set of possible
values. A `number` describes every possible number, so a single number
(such as `42`) would be a *subtype* of the `number` type.

If we want to know if one type is the subtype of another. We need to look at
all the possible values for both types and figure out if the other has a
_subset_ of the values.

For example, if we had a `TypeA` which described the numbers 1 through 3, and
a `TypeB` which described the numbers 1 through 5. `TypeB` would be considered
a _subtype_ of `TypeA` because `TypeA` is a subset of `TypeB`.

```js
type TypeA = 1 | 2 | 3;
type TypeB = 1 | 2 | 3 | 4 | 5;
```

> **Note:** To avoid any confusion remember that the relationships between
> "subsets" and "subtypes" are inverted. (i.e. `X` is a subtype of `Y` only if
> `Y` is a subset of `X`).

If we had a `TypeLetters` which described the strings: "A", "B", "C", and a
type `TypeNumbers` which described the numbers: 1, 2, 3. Neither of them would
be a subtype of the other, as they contain a completely different set of
values.

```js
type TypeLetters = "A" | "B" | "C";
type TypeNumbers =  1  |  2  |  3;
```

Finally, if we has a `TypeA` which described the numbers 1 through 3, and a
`TypeB` which described the numbers 3 through 5. Neither of them would be a
subtype of the other. Even though they both have 3 and describe numbers, they
each have some unique items.

```js
type TypeA = 1 | 2 | 3;
type TypeB =         3 | 4 | 5;
```

## When are subtypes used? <a class="toc" id="toc-when-are-subtypes-used" href="#toc-when-are-subtypes-used"></a>

Most of the work that Flow does is comparing types against one another.

For example, in order to know if you are calling a function correctly. Flow
needs to compare the arguments you are passing to the function with the
parameters the function expects.

This often means figuring out if the value you are passing in is a subtype of
the value you are expecting.

So if I write a function that expects the numbers 1 through 5, any subtype of
that set will be acceptable.

```js
// @flow
function method(val: 1 | 2 | 3 | 4 | 5) {
  // ...
}

declare var numsA:  1 |  2;
declare var numsB: 42 | 75;

method(numsA); // Works!
// $ExpectError
method(numsB); // Error!
```

## Subtypes of complex types <a class="toc" id="toc-subtypes-of-complex-types" href="#toc-subtypes-of-complex-types"></a>

Flow needs to compare more than just sets of primitive values, it also needs to
be able to compare objects, function, and everything that appears in the
language.

#### Subtypes of objects <a class="toc" id="toc-subtypes-of-objects" href="#toc-subtypes-of-objects"></a>

You can start to compare two objects by their keys. If one object contains all
the keys of another object, then it may be a subtype.

For example, if we had an `ObjectA` which contained the key `foo`, and an
`ObjectB` which contained the keys `foo` and `bar`. Then it's possible that
`ObjectA` is a subtype of `ObjectB`.

```js
// @flow
type ObjectA = { foo: string };
type ObjectB = { foo: string, bar: number };

let objectB: ObjectB = { foo: 'test', bar: 42 };
let objectA: ObjectA = objectB; // Works!
```

But we also need to compare the types of the values. If both objects had a key
`foo` but one was a `number` and the other was a `string`, then one would not
be the subtype of the other.

```js
// @flow
type ObjectA = { foo: string };
type ObjectB = { foo: number, bar: number };

let objectB: ObjectB = { foo: 1, bar: 2 };
// $ExpectError
let objectA: ObjectA = objectB; // Error!
```

If these values on the object happen to be other objects, we would have to
compare those against one another. Comparing every value recursively until we
can decide if we have a subtype or not.

#### Subtypes of functions <a class="toc" id="toc-subtypes-of-functions" href="#toc-subtypes-of-functions"></a>

Flow compares two functions by comparing its inputs and outputs. If all the
inputs and outputs are a subset of the other function, then it is a subtype.

```js
type Func1 = (1 | 2)     => "A" | "B";
type Func2 = (1 | 2 | 3) => "A" | "B" | "C";
```

This also applies to the number of parameters in the functions. If one function
contains a subset of the parameters of the other then the other is a subtype.

```js
// @flow
type Func1 = (number) => void;
type Func2 = (number, string) => void;

let func1: Func1 = (a: number) => {};
let func2: Func2 = func1;
```
