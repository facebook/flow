---
id: operators
title: Operators
layout: docs
permalink: /docs/operators.html
prev: functions.html
---

Most of JavaScript's operators are heavily overloaded, and involve various 
implicit conversions. Flow supports common uses of these operators, while 
banning others that are common causes of errors.

Some of the interesting restrictions that Flow imposes are:

- `+` either takes two `number`s and gives back a `number`, or takes a `string`
  and anything else and gives back a `string`. JavaScript specifies a more
  elaborate sequence of implicit conversions (e.g., `Date` objects and 
  `boolean`s may be converted to `number`s), but Flow doesn't support them.

- `==` and `!=` take either two values of the same base type (modulo subtyping 
with `mixed`, `void`, and `any`), or two values of non-base types, and give 
back a `boolean`. Specifically, these operators cannot compare two values of 
incompatible base types, or a value of a base type with a value of non-base 
type. JavaScript specifies an elaborate sequence of implicit conversions to 
base types in these cases, which may cause weird errors. The alternative is to 
use `===` and `!==` instead, which perform direct checks without implicit 
conversions.
