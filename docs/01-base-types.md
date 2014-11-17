---
id: base-types
title: Base Types
layout: docs
permalink: /docs/base-types.html
prev: type-annotations.html
next: variables.html
---

There are several base types used by Flow for type annotations.

## JavaScript Base Types

The following base types describe values that come out of the JavaScript
runtime.

- **`number`**: the numeric type (e.g., hex, int, double). This type will be
the result of most arithmetic and bitwise operations.
- **`string`**: the string type.
- **`boolean`**: the type representing `true` and `false`. This type will be
result of most logical operations.
- **`void`**: the type representing `undefined`.
- **`function`**: the type of any function.

## Constructor Names

Class names and function names can be used as annotations as well. These types
describe the objects that are constructed with them.

## Other Base Types

Flow has added some base types to use for annotations.

- **`mixed`**: the "supertype" of all types. Any type can flow into a `mixed`.
- **`any`**: the "dynamic" type. Any type can flow into `any`, and vice-versa

## Base Type Notes and Caveats

- The classes `Number`, `String`, and `Boolean` can be used as type 
annotations (just like other classes). However, these classes describe objects 
rather than base values, and, as such, are incomparable to the base types
`number`, `string`, and `boolean`. The methods of the class types are available
to the base types by default, however.
- The classes `Object` and `Function` can be also used as type annotations 
(just like other classes). `Function` means the same thing as `function`.
There is no corresponding type named `object`. 
- There are many implicit conversions to `boolean` in JavaScript. Flow
understands this behavior and allows it. For example, any expression can be
used as a conditional in an `if`-statement, or as an operand of the `&&`
operation.
- The addition `+` operator works on `number`s **and** `string`s. In the case
of a using the operator on a `number` and `string`, the `number` is implictly
converted to a `string` via `toString()` and a concatenation takes place. Flow
recognizes and allows this behavior.
- `Date` objects implicitly convert to `number` whenused in arithmetic operations. 
Flow supports this behavior.
-  Use `void` to annotate a function return when it returns nothing, but do 
not use it for anything else! The only values that have type `void` are 
undefined values such as `undefined` and results of the operator `void()`. Flow is
fairly strict about when `void` can flow into.
- Use `mixed` to annotate a location that can take anything, but 
do not use `Object` instead! It is confusing to view everything as an object, 
and if by any chance you do mean "any object", there is a better way to 
specify that, just as there is a way to specify "any function". Also, be aware 
of the difference between `mixed` and `any`: `mixed` is a taint that 
propagates quickly, since no useful operation can be performed on it.

## `any`

It is worth calling out `any` specifically because of the special nature of
this annotation. Use any to escape the static typing of Flow. In other words, 
if Flow is getting in your way, and you are absolutely convinced your program 
is type correct, you can silence the errors by annotating locations along the 
error paths with type `any`.

Using this "backdoor" is dangerous and not recommended, but it is necessary 
because a static type system simply cannot model the entirety of a dynamic
language. Furthermore, it is a necessary tool for gradual typing, in that 
`any`models code where Flow does not have knowledge; i.e., when you reference 
something in a non-Flow file, it has type `any`.
