---
layout: guide
---

### Too Few Arguments

When you call a function with fewer arguments than it accepts, the `void` type
will be flowed to the missing parameters. If the missing parameter does not
accept values of type `void` then you will get an error.

```js
// @flow
function method(value: number) {
  // ...
}

method(); // Error!
```

However, if the missing parameter accepts values of type `void` then there will
be no error.

```js
// @flow
function method(value: number | void) {
  // ...
}

method(); // Works!
```

### Too Many Arguments

In JavaScript you can call a function with more arguments than it expects. Flow
allows this too. However, there is an easy trick to declare a function can’t
take extra arguments.

```js
// @flow
function method(value: number, ...rest: Array<empty>) {
  // ...
}

method(1, 2); // Error!
```

This is particularly useful when declaring overloads in library definitions.

```js
// @flow
declare function method(...rest: Array<empty>): string;
declare function method(a: number, ...rest: Array<empty>): string;
declare function method(a: number, b: number, ...rest: Array<empty>): string;
```
