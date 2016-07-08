---
id: destructuring
title: Destructuring
permalink: /docs/destructuring.html
prev: nullable-types.html
next: type-aliases.html
---

Flow supports the JavaScript construct of destructuring, which allows
you to extract data from structured values. Here is a simple example:

```js +line_numbers
/* @flow */
var arr = [1, '', true];
var [a, b, c] = arr;
// a: number (1), b: string (''), c : boolean (true)
```

The canonical example of destructuring is swapping:

```js +line_numbers
var a = 1, b = 2;
[a, b] = [b, a];
// a = 2, b = 1
```

## Destructuring and Type Checks

Flow can verify that any destructuring in your code is type-safe.

```js +line_numbers
/* @flow */
var arr = [1, '', 'Hello', true];
// If you only care about some of the return values, you can skip some
// elements with , ,
var [a, b, ,c] = arr;
// a: number (1), b: string (''), c : boolean (true)
var z: number = a * c;
```

Above we have a four (4) element array `arr` (actually a
[`tuple`](http://flowtype.org/docs/arrays.html#tuples))
. And then we destructure that array into three (3) variables, `a`, `b`,  `c`.
However, we then try to multiply `a` (a `number`), and `c` (a `boolean`). Flow
catches this.

```text
/tmp/flow/f.js:2:28,31: boolean
This type is incompatible with
  /tmp/flow/f.js:5:17,21: number

Found 1 error
```
{: .cli-error}

## Another Example

```js +line_numbers
/* @flow */
var {x, y, ...o} = {x: '', y: 3, o: {z: false} }
// x: string, y: number, o: {z: boolean}
var z: number = o;
```

```text
/tmp/flow/f.js:3:5,16: object pattern
This type is incompatible with
  /tmp/flow/f.js:5:8,13: number

Found 1 errors
```
{: .cli-error}

`o` has been destructed as an object that contains a boolean value. That
cannot be assigned to a number.
