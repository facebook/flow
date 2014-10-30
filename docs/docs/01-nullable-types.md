---
id: nullable-types
title: Nullable Types
layout: docs
permalink: /docs/nullable-types.html
prev: objects.html
next: functions.html
---

In JavaScript, `null` implicitly converts to all the primitive types; it is 
also a valid inhabitant of any object type.

In contrast, Flow considers `null` to be a distinct value that is not part of 
any other type. For example, the following code does not typecheck:

```javascript
/* @flow */
var o = null;
print(o.x);
```

## Type Annotating Null

Any type `T` can be made //nullable// by writing `?T`: the latter type 
describes `null` or the set of values of `T`.

```javascript
var o: ?string = null;
print(o.x);
```

Making a type nullable makes it a valid annotation for any location that may 
contain `null`, but it still does not allow useful operations to be performed 
on it (as shown by the code above). To do so, we must perform a `null` check, 
as follows:

```javascript
/* @flow */
var o: ?string = null;
if (o == null) o = { x: "hello" };
print(o.x);
```

In this code, after the `if`-statement Flow infers that `o` is not `null` (it 
either was `null` before the `if`-statement but is now an object, or was not 
`null` before the `if`-statement). So the code typechecks.

This illustrates an interesting feature of Flow: it understands the effects of 
some dynamic type tests and can adjust the types of local variables 
accordingly. (In technical terms, Flow's analysis is //path-sensitive//.) 

## Nullable and Objects

While Flow can adjust types of local variables, it cannot adjust types of 
objects for reasons mentioned above. In particular, don't expect a nullable 
field to be recognized as non-`null` in some method because a `null` check is 
performed in some other method in your code, even when it is clear to you that 
the `null` check is sufficient for safety at run time (say, because you know 
that calls to the former method always follow calls to the latter method). On 
the other hand, you can always propagate the result of a `null`-check by 
explicitly passing around the non-`null` value in your code, and if you are 
careful enough it should be possible to satisfy Flow without doing additional 
`null` checks.

## Undefined Values

Undefined values, just like `null`, can cause issues too. Unfortunately, 
undefined values are ubiquitous in JavaScript and it is hard to avoid them 
without severely affecting the usability of the language. For example, arrays 
can have holes for elements; object properties can be dynamically added and 
removed. If Flow did not allow undefined values to be part of all types, you 
would have to do undefined checks (like `null` checks) on each dereference of 
an array element or object property to get anything useful done. 
