---
id: operators
title: Operators
permalink: /docs/operators.html
---

Most of JavaScript's operators are heavily overloaded, and involve various 
implicit conversions. Flow supports common uses of these operators, while 
banning others that are common causes of errors.

Some of the interesting restrictions that Flow imposes are:

- `+` either takes two `number`s and gives back a `number`, or takes a `string`
  and anything else and gives back a `string`. In addition, JavaScript specifies 
some implicit conversions that happen as part of this operation (e.g., `Date` objects and 
  `boolean`s may be converted to `number`s), which Flow understands.

- `==` and `!=` take either two values of the same base type (modulo subtyping 
with `mixed`, `void`, and `any`), or two values of non-base types, and give 
back a `boolean`. Specifically, these operators cannot compare two values of 
incompatible base types, or a value of a base type with a value of non-base 
type. JavaScript specifies an elaborate sequence of implicit conversions to 
base types in these cases, which may cause weird errors. The alternative is to 
use `===` and `!==` instead, which perform direct checks without implicit 
conversions. The only exception to this rule is that `null` and `undefined`
should be compared to other types with `==` and `!=`, instead of `===` and `!==`,
since the non-strict equality considers `null` and `undefined` to be the same,
and distinct from all other values. Such checks are therefore useful to 
narrow down maybe types.
