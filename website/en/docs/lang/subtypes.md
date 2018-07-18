---
layout: guide
---

## What is a subtype? <a class="toc" id="toc-what-is-a-subtype" href="#toc-what-is-a-subtype"></a>

A type like `number`, `boolean`, or `string` describes a set of possible
values. A `number` describes every possible number, so a single number
(such as `42`) would be a *subtype* of the `number` type.

If we want to know whether one type is the subtype of another, we need to look at
all the possible values for both types and figure out if the other has a
_subset_ of the values.

For example, if we had a `TypeA` which described the numbers 1 through 3, and
a `TypeB` which described the numbers 1 through 5: `TypeA` would be considered
a _subtype_ of `TypeB`, because `TypeA` is a subset of `TypeB`.

```js
type TypeA = 1 | 2 | 3;
type TypeB = 1 | 2 | 3 | 4 | 5;
```

Consider a `TypeLetters` which described the strings: "A", "B", "C", and a
`TypeNumbers` which described the numbers: 1, 2, 3. Neither of them would
be a subtype of the other, as they each contain a completely different set of
values.

```js
type TypeLetters = "A" | "B" | "C";
type TypeNumbers =  1  |  2  |  3;
```

Finally, if we had a `TypeA` which described the numbers 1 through 3, and a
`TypeB` which described the numbers 3 through 5. Neither of them would be a
subtype of the other. Even though they both have 3 and describe numbers, they
each have some unique items.

```js
type TypeA = 1 | 2 | 3;
type TypeB = 3 | 4 | 5;
```

## When are subtypes used? <a class="toc" id="toc-when-are-subtypes-used" href="#toc-when-are-subtypes-used"></a>

Most of the work that Flow does is comparing types against one another.

For example, in order to know if you are calling a function correctly, Flow
needs to compare the arguments you are passing with the parameters the
function expects.

This often means figuring out if the value you are passing in is a subtype of
the value you are expecting.

So if I write a function that expects the numbers 1 through 5, any subtype of
that set will be acceptable.

```js
// @flow
function f(param: 1 | 2 | 3 | 4 | 5) {
  // ...
}

declare var oneOrTwo: 1 |  2; // Subset of the input parameters type.
declare var fiveOrSix: 5 | 6; // Not a subset of the input parameters type.

f(oneOrTwo); // Works!
// $ExpectError
f(fiveOrSix); // Error!
```

## Subtypes of complex types <a class="toc" id="toc-subtypes-of-complex-types" href="#toc-subtypes-of-complex-types"></a>

Flow needs to compare more than just sets of primitive values, it also needs to
be able to compare objects, functions, and every other type that appears in the
language.

#### Subtypes of objects <a class="toc" id="toc-subtypes-of-objects" href="#toc-subtypes-of-objects"></a>

You can start to compare two objects by their keys. If one object contains all
the keys of another object, then it may be a subtype.

For example, if we had an `ObjectA` which contained the key `foo`, and an
`ObjectB` which contained the keys `foo` and `bar`. Then it's possible that
`ObjectB` is a subtype of `ObjectA`.

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
compare those against one another. We need to compare every value 
recursively until we can decide if we have a subtype or not.

#### Subtypes of functions <a class="toc" id="toc-subtypes-of-functions" href="#toc-subtypes-of-functions"></a>

Subtyping rules for functions are more complicated. So far, we've seen that `A`
is a subtype of `B` if `B` contains all possible values for `A`. For functions,
it's not clear how this relationship would apply. To simplify things, you can think
of a function type `A` as being a subtype of a function type `B` if functions of type
`A` can be used wherever a function of type `B` is expected.

Let's say we have a function type and a few functions. Which of the functions can
be used safely in code that expects the given function type?

```js
type FuncType = (1 | 2) => "A" | "B";

let f1: (1 | 2) => "A" | "B" | "C" = (x) => /* ... */
let f2: (1 | null) => "A" | "B" = (x) => /* ... */
let f3: (1 | 2 | 3) => "A" = (x) => /* ... */
```

- `f1` can return a value that `FuncType` never does, so code that relies on `FuncType`
might not be safe if `f1` is used. Its type is not a subtype of `FuncType`.
- `f2` can't handle all the argument values that `FuncType` does, so code that relies on
`FuncType` can't safely use `f2`. Its type is also not a subtype of `FuncType`.
- `f3` can accept all the argument values that `FuncType` does, and only returns
values that `FuncType` does, so its type is a subtype of `FuncType`.

In general, the function subtyping rule is this: A function type `B` is a subtype
of a function type `A` if and only if `B`'s inputs are a superset of `A`'s, and `B`'s outputs
are a subset of `A`'s. The subtype must accept _at least_ the same inputs as its parent,
and must return _at most_ the same outputs.

The decision of which direction to apply the subtyping rule on inputs and outputs is
governed by variance, which is the topic of the next section.
