---
id: type-annotations
title: Type Annotations
permalink: /docs/type-annotations.html
prev: existing.html
next: third-party.html
---

JavaScript is inherently a dynamically-typed language. As such, explicitly
typing your code is not part of the JavaScript lexicon. This is normal
JavaScript code:

```js +line_numbers
function add(num1, num2) {
  return num1 + num2;
}
var x = add(3, '0');
console.log(x);
```

What is the value of `x`? `3`? `"30"`? `undefined`? The answer is `"30"`, and, in most
cases, this probably not the behavior you would prefer.

Flow helps mitigate these sort of subtle bugs by trying to keep your code sane
through static analysis and type annotations.

## Type Annotations

Type annotations are generally prefixed by `:`. And they can be placed on
function parameters, function return types and variable declarations. e.g.,

```js +line_numbers
function foo(a: string, b: number): void { ... }
var x: boolean = someBool;
class Bar {
  y: string;
  someMethod(a: number): string { ... }
}
```

## Simple Example

We can easily take this code and make it Flow aware by adding a simple
annotation `@flow` at the top in a comment block:

```js +line_numbers
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

```js +line_numbers
/* @flow */
function add(num1: number, num2: number): number {
  return num1 + num2;
}
var x: number = add(3, '0');
console.log(x);
```

Running the type checker against the above code will yield type errors
since we have explicitly typed all parameters and variables.

```text
file.js:5
  5: var x: number = add(3, '0');
                     ^^^^^^^^^^^ function call
  5: var x: number = add(3, '0');
                            ^^^ string. This type is incompatible with
  2: function add(num1: number, num2: number): number {
                                      ^^^^^^ number

Found 1 error
```
{: .cli-error}

## Type Annotation Requirements

Type annotations are not always strictly necessary to use Flow. As shown above,
all that is strictly required to make your JavaScript file Flow aware is
the `@flow` annotation. And this annotation by itself can be enough for Flow to
deduce all that is necessary to type check your code.

```js +line_numbers
/* @flow */
function multPI(num1, num2) {
  return Math.PI * num1 * num2;
}
var x = multPI(3, '0');
console.log(x);
```

Since the multiplication operator makes no real sense on a string, Flow is
smart enough to deduce a problem here without explicit type annotations.

```text
file.js:5
  5: var x = multPI(3, '0');
             ^^^^^^^^^^^^^^ function call
  3:   return Math.PI * num1 * num2;
                               ^^^^ string. This type is incompatible with
  3:   return Math.PI * num1 * num2;
              ^^^^^^^^^^^^^^^^^^^^^ number

Found 1 error
```
{: .cli-error}

### Module Boundaries

Flow requires annotations at the boundaries of modules. This allows Flow to analyze modules in isolation which improves the performance of checking types across module boundaries. We've found that this helps to improve the self-documenting nature of module interfaces as well.

```js +line_numbers
/**
 * Size.js
 * @flow
 */
function size(input: string): number {
  return input.length;
}

module.exports = size;
```

```js +line_numbers
/**
 * UseSize.js
 * @flow
 */
var size = require('./Size');
var result = size(null);
```

Type annotations are required for the `size` function in `Size.js` because `UseSize.js` imports it and thus crosses the module boundary and isn't inferred.

```text
UseSize.js:6
  6: var result = size(null);
                  ^^^^^^^^^^ function call
  6: var result = size(null);
                       ^^^^ null. This type is incompatible with
  5: function size(input: string): number {
                          ^^^^^^ string. See: Size.js:5

Found 1 error
```
{: .cli-error}

## `any` Annotations

`any` is a special type annotation that represents the universal dynamic type.
`any` can flow to any other type, and vice-versa. `any` is basically the "get
out of my way, I know what I am doing" annotation. Use it when Flow is getting
in your way, but you know your program is correct.

## Bottom Line

You can type annotate all your code. That would be the most expressive and
self-documenting approach. However, Flow does a lot of type inference for you to
alleviate this requirement when it becomes a burden. The only place that you must
annotate types is where those types go across module boundaries.
