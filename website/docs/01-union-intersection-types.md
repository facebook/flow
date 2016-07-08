---
id: union-intersection-types
title: Union and Intersection Types
permalink: /docs/union-intersection-types.html
prev: type-aliases.html
next: typeof.html
---

Flow adds support for both union and intersection types. A union type requires
for a value to be one of the input types.

```js +line_numbers
/* @flow */
type U = number | string;
var x: U = 1;
x = "two";
```

An intersection type requires a value to be all of the input types:

```js +line_numbers
/* @flow */
type I = {a: number} & {b: number};
var x: I = {a: 1, b: 2};
x = {a: 1, b: 2, c: "three"};
```

The value `{a: 1, b: 2, c: "three"}` is admissible here because the
`I` only requires that properties `a` and `b` are present.

## Syntax

- Union: `<type 1> | <type 2>  ... | <type n>`
- Intersection: `<type 1> & <type 2> ... & <type n>`

## Union Example

```js +line_numbers
/* @flow */
class A {}
class B {}
class C {}

var x: A | B | number | C = new C();
x = 3;
x = new B();
x = true; // Flow will error here
```

`x` is the union of `A`, `B`, `number` and `C`. So `x` can be assigned to any
of those types. It cannot, however, be assigned a `boolean`.

```text
/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:2:7,7: A

/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:3:7,7: B

/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:4:7,7: C

/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:6:16,21: number
```
{: .cli-error}

## Intersection Example

```js +line_numbers
/* @flow */
class Foo {}
class Bar {}
declare var f: ((x: Foo) => void) & ((x: Bar) => void);
f(new Foo());
f(true); // Flow will error here.
```

`f` is intersected on `function` that take a `Foo` or `Bar`. Trying to pass in
a `boolean` will cause a type error.

```text
/tmp/flow/f.js:6:3,6: boolean
This type is incompatible with
  /tmp/flow/f.js:2:7,9: Foo

/tmp/flow/f.js:6:3,6: boolean
This type is incompatible with
  /tmp/flow/f.js:3:7,9: Bar
```
{: .cli-error}
