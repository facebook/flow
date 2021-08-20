---
title: Variable Types
slug: /types/variables
---

When you are declaring a new variable, you may optionally declare its type.

JavaScript has three ways of declaring local variables:

- `var` - declares a variable, optionally assigning a value.
  ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var))
- `let` - declares a block-scoped variable, optionally assigning a value.
  ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let))
- `const` - declares a block-scoped variable, assigning a value that cannot be re-assigned.
  ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/const))

In Flow these fall into two groups:

- `let` and `var` - variables that **can** be reassigned.
- `const` - variables that **cannot** be reassigned.

```js flow-check
// @flow
var varVariable = 1;
let letVariable = 1;
const constVariable = 1;

varVariable = 2;   // Works!
letVariable = 2;   // Works!
// $ExpectError
constVariable = 2; // Error!
```

## `const` {#toc-const}

Since a `const` variable cannot be re-assigned at a later time it is fairly
simple.

Flow can either infer the type from the value you are assigning to it or you
can provide it with a type.

```js flow-check
// @flow
const foo /* : number */ = 1;
const bar: number = 2;
```

## `var` and `let` {#toc-var-and-let}

Since `var` and `let` can be re-assigned, there's a few more rules you'll need
to know about.

Similar to `const`, Flow can either infer the type from the value you are
assigning to it or you can provide it with a type:

```js flow-check
// @flow
var fooVar /* : number */ = 1;
let fooLet /* : number */ = 1;
var barVar: number = 2;
let barLet: number = 2;
```

When you provide a type, you will be able to re-assign the value, but it must
always be of a compatible type.

```js flow-check
// @flow
let foo: number = 1;
foo = 2;   // Works!
// $ExpectError
foo = "3"; // Error!
```

When you do not provide a type, the inferred type will do one of two things if
you re-assign it.

## Reassigning variables {#toc-reassigning-variables}

By default when you re-assign a variable, Flow will give it the type of all
possible assignments.

```js
let foo = 42;

if (Math.random()) foo = true;
if (Math.random()) foo = "hello";

let isOneOf: number | boolean | string = foo; // Works!
```

Sometimes Flow is able to figure out (with certainty) the type of a variable
after re-assignment. In that case, Flow will give it the known type.

```js flow-check
// @flow
let foo = 42;
let isNumber: number = foo; // Works!

foo = true;
let isBoolean: boolean = foo; // Works!

foo = "hello";
let isString: string = foo; // Works!
```

If statements, functions, and other conditionally run code can all prevent Flow
from being able to figure out precisely what a type will be.

```js flow-check
// @flow
let foo = 42;

function mutate() {
  foo = true;
  foo = "hello";
}

mutate();

// $ExpectError
let isString: string = foo; // Error!
```

As Flow gets smarter and smarter, there should be fewer instances of these scenarios.
