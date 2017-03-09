---
id: type-aliases
title: Type Aliases
permalink: /docs/type-aliases.html
prev: destructuring.html
next: union-intersection-types.html
---

Flow supports type aliasing.
Type aliases are similar to `typedef`s in C or type abbreviations in OCaml. Type aliasing
provides a way to redefine existing types as new type names. For example,
type aliases may be used to define names for object types, effectively modeling
interface types.

Here is a simple example:

```js +line_numbers
/* @flow */
type T = number;
var x: T = 0;
```

We declare the new type `T` is an alias for the built-in type `number`.
Anywhere we use `T`, we are asserting that `T` will have an underlying
type of `number`.

## Type Checking Aliases

Aliases are type checked the same way as any other type.

```js +line_numbers
/* @flow */
type T = Array<string>;
var x: T = [];
x["Hi"] = 2;
```

```text
/tmp/flow/f.js:4:3,6: string
This type is incompatible with
  /tmp/flow/f.js:4:1,11: number

/tmp/flow/f.js:4:11,11: number
This type is incompatible with
  /tmp/flow/f.js:2:16,21: string

Found 2 errors
```
{: .cli-error}

As you can see, we aliased an `Array<string>` to a new type called `T`. And
then we tried to assign a `number` value to a `string` key in the array.
However, that does not comport with how we declared `T`. Instead, this would
work:

```js +line_numbers
/* @flow */
type T = Array<string>;
var x: T = [];
x[2] = "Hi";
```

## A More Complicated Example

Let's take a look at a more involved example where we use arrow functions in
our type aliasing.

```js +line_numbers
/* @flow */
// Let F<U, V> describe the type of functions of the form
// function(x: U) {
//   // return some value compatible with that assigned to V
// }
type F<U, V> = (x: U) => V;

// The function foo applies a given function f to a given argument x
function foo<X, Y>(f: F<X, Y>, x: X): Y { return f(x); }
var b: boolean = true;
var result: string = foo (function(x) { return b; }, 0);
```

We alias a function (via the `=>` syntax), to `F<U, V>`. So whenever `F<U, V>`
is used as a parameter or return type, that function will be the underlying
type.

Then we create a function that is parameterized to `X` and `Y`. `foo()` takes
as its first parameter our type alias `F<X, Y>` and as its second parameter a
value of type `X`. We are returning a `Y` from `foo()`.

When we call `foo()` we have assigned `X` to an `int` (via `x`) and `Y` as
a `boolean` (via `b`). Thus, `result` is expected to be of type `boolean`.

Since we have `result` as a `string`, we get the following error:

```text
/tmp/flow/f.js:9:48,48: boolean
This type is incompatible with
  /tmp/flow/f.js:9:13,18: string

Found 1 error
```
{: .cli-error}
