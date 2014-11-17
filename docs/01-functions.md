---
id: functions
title: Functions
layout: docs
permalink: /docs/functions.html
prev: primitives.html
next: operators.html
---

Functions are ubiquitous in JavaScript. As expected, Flow propagates types through function calls.

## Type Annotating Functions

```javascript
/* @flow */
function foo(x: string): string { return x; }
var x: number = foo('');
```

Running Flow produces the following error:

```bbcode
File "example.js", line 3, character 20-21:
string
is incompatible with
File "example.js", line 3, characters 7-12:
number
```

## Open methods

In JavaScript, functions also take an implicit `this` parameter, and can
therefore serve as open methods for objects that have such functions as
properties: the `this` parameter is bound to whatever object the method is
called on. Flow understands such behavior and propagates types through `this`
as well. For example, the following code does not typecheck:

```javascript
/* @flow */
function foo(x) { return this.x; }
var o = { x: 42, f: foo };
var x: string = o.f();
```

```bbcode
testcode/flow/openMethods.js:3:14,15: number
This type is incompatible with
  testcode/flow/openMethods.js:4:8,13: string

testcode/flow/openMethods.js:4:17,21: call of method f
Too few arguments
  testcode/flow/openMethods.js:2:1,34: function
```


## Variadics

Functions can take optional and rest parameters, and calls to such functions
are checked as expected. For example, the following code typechecks:

```javascript
/* @flow */
function foo(x, y = false) { }
function bar(z, ...w) { }
foo(0);
bar('h', 'e', 'l', 'l', 'o');
```

Calls are matched against function signatures following the usual rules of
argument matching while taking into account optional/rest parameters.

### Too Few Arguments

Flow produces an error whenever a call provides too few arguments to a
function. On the other hand, too many arguments is fine, which is why Flow
does not complain on most uses of builtin primitives like `Array`, whose
methods are often called with closures that take fewer parameters than
prescribed.

## Function-based type annotations

Since functions are first-class values in JavaScript (meaning they can be
passed around, like numbers), type annotations may include function types. A
function type is of the form `(P1: T1, .., Pn: Tn) => U` where each `Ti` is a
parameter type, `U` is the return type, and each `Pi` is one of the following:

- an identifier `x`, suggesting a name for a regular parameter
- of the form `x?`, indicating an optional parameter
- of the form `...x`, indicating a rest parameter

There may be at most one rest parameter, which has to appear at the end, and
optional parameters must follow regular parameters.

Furthermore, function expressions and function definitions may have parts of
their types annotated inline, as seen above. For example, we may have:

`function foo (P1: T1, .., Pn: Tn): U { .. }`

## Polymorphic functions
Functions can be polymorphic, just like polymorphic classes.

```javascript
/* @flow */
function foo<X>(x: X): X { return x; }

var x: number = foo(0);
var y: string = foo('');
```

Furthermore, you may have polymorphic methods in polymorphic classes. For
example, you may define a List class with a map method:

```javascript
/* @flow */
class List<T> {
  ...
  map<U>(f: (x: T) => U): List<U> { ... }
}
```

This means that for every instantiation of `T`, there is a polymorphic method
for objects of type `List<T>` that, for any instantiation of `U`, takes a
function of type `(x: T) => U` and returns an object of type `List<U>`.

## Overloading

Some methods, such as `replace()` in `String` and (the polymorphic method)
`then()` in (the polymorphic class) `Promise`, have multiple signatures to
model slightly different use cases that otherwise make sense to group into a single method.

Flow understands such "overloaded" signatures and knows how to apply the
correct one for a given call. In fact, the addition operator is a special case
of an overloaded function that returns number in some cases and string in
others, based on the types of its arguments.

To express such signatures, Flow provides a syntax for union types:

`$Either<T1, .., Tn>`

is the union of types `Ti`. Union types are available for general use. (The
syntax is designed to reuse existing machinery in the parser; a better syntax
for union types may be adopted in the future.)

As specific cases, the overloaded signatures of both `replace()` in `String` and `then()` in `Promise` have been rewritten to use union types, thereby compressing a combinatorial number of signatures into one. In general this is possible whenever return types do not depend on the specific choice and combination of argument types, which is often the case in JavaScript due to lack of overloading support at run time.

### Overloading Caution

Overloading is not recommended in general in a dynamic language, because it
can be very confusing, and often results in performance penalties. Flow does not yet provide a way to declare overloaded signatures for definitions outside the prelude, so it is not available for general use.

Curiously, there is no actual overloading at run time in JavaScript (since
there are no static types at run time). Instead, an overloaded function is
implemented by a function that accepts several possible arguments and then
does a series of dynamic type tests in its body to dispatch accordingly. This
has an interesting effect: since type signatures reflect the truth about
implementations, it often turns out that a set of overloaded signatures can be
simplified to a single signature using a union type for some parameters.

## Recursive Union Types

Flow does not yet understand recursive union types, i.e., the "cases" of a
union type cannot refer back to the union type. Recursive union types can be
used to succinctly model recursive data structures such as lists and trees,
which are quite useful to describe things like queries, UI nodes, etc. It
shouldn't be too hard to add this feature to Flow if there is enough demand
for it.
