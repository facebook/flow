---
id: type-annotations
title: Type Annotations
layout: docs
permalink: /docs/type-annotations.html
prev: troubleshooting.html
next: base-types.html
---

Javascript is inherently a dynamically-typed language. As such, explicitly
typing your code is not part of the JavaScript lexicon. This is normal
JavaScript code:

```javascript
function add(num1, num2) {
  return num1 + num2;
}
var x = add(3, '0');
console.log(x);
```

What is the value of `x`? `3`? `30`? `undefined`? The answer is `30`, and, in most
cases, this probably not the behavior you would prefer.

Flow helps mitgate these sort of subtle bugs by trying to keep your code sane
through static analysis and type annotations.

## Type Annotations

Type annotations are generally prefixed by `:`. And they can be placed on
function parameters, function return types and variable declarations. e.g.,

```javascript
function foo(a: mixed, b: number): void {...}
var x: boolean;
class Bar {
  y: string;
}
```

## Simple Example

We can easily take this code and make it //Flow aware// by adding a simple
annotation `@flow` at the top in a comment block:

```javascript
/* @flow */
function add(num1, num2) {
  return num1 + num2;
}
var x = add(3, '0');
console.log(x);
```

However, Flow will find no errors with the above code. That's because the `+`
operator is perfectly acceptable on `number`s and `string`s, and we didn't
specify that the parameters to `add` must be `number`s.

```javascript
/* @flow */
function add(num1: number, num2: number): number {
  return num1 + num2;
}
var x: number = add(3, '0');
console.log(x);
```

Running the type checker against the above code //will// yield type errors
since we have explicitly typed all parameters and variables.

```
testcode/flow/easyflow.js:7:24,26: string
This type is incompatible with
  testcode/flow/easyflow.js:4:34,39: number
```

## Type Annotation Requirements

Type annotations are not always strictly necessary to use Flow. As shown above,
all that is strictly required to make your JavaScript file //Flow aware// is
the `@flow` annotation. And this annotation by itself can be enough for Flow to
deduce all that is necessary to type check your code.

```javascript
/* @flow */
function multPI(num1, num2) {
  return Math.PI * num1 * num2;
}
var x = multPI(3, '0');
console.log(x);
```

Since the multiplication operator makes no real sense on a string, Flow is
smart enough to deduce a problem here without explicit type annotations.

```
testcode/flow/makeflow.js:7:19,21: string
This type is incompatible with
  testcode/flow/makeflow.js:5:10,30: number
```

### Module Boundaries

However, explicit type annotations are required at all module boundaries.
Flow's inference engine stops there.

```javascript
/**
 * Size.js
 * @flow
 */
function size(input: string): number {
  return input.length;
}

module.exports = size;
```

```javascript
/**
 * UseSize.js
 * @flow
 */
var size = require('./Size');
var result = size(null);
```

Type annotations are required in `Size.js` because `UseSize.js` is calling the
`size()` function from outside the module and that crosses the inference
boundary.

```
testcode/flow/UseSize.js:7:19,22: null
This type is incompatible with
  testcode/flow/Size.js:5:22,27: string
```

## `any`

`any` is a special type annotation that represents the universal dynamic type.
`any` can flow to any other type, and vice-versa. `any` is basically the "get
out of my way, I know what I am doing" annotation. Use it when Flow is getting
in your way, but you know your program is correct.

## Bottom Line

You can type annotate all your code. That would be the most expressive and
self-documenting approach. However, type annotations are only required across
module boundaries as the Flow type inference engine is inner-module aware only.
