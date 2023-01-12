---
title: Intersection Types
slug: /types/intersections
---

Sometimes it is useful to create a type which is ***all of*** a set of other
types. For example, you might want to write a function which accepts an object
which is the combination of other object types. For this, Flow supports
**intersection types**.

```js flow-check
// @flow
type A = { a: number };
type B = { b: boolean };
type C = { c: string };

function method(value: A & B & C) {
  // ...
}

// $ExpectError
method({ a: 1 }); // Error!
// $ExpectError
method({ a: 1, b: true }); // Error!
method({ a: 1, b: true, c: 'three' }); // Works!
```

## Intersection type syntax {#toc-intersection-type-syntax}

Intersection types are any number of types which are joined by an ampersand `&`.

```js
Type1 & Type2 & ... & TypeN
```

You may also add a leading ampersand which is useful when breaking intersection
types onto multiple lines.

```js
type Foo =
  & Type1
  & Type2
  & ...
  & TypeN
```

Each of the members of a intersection type can be any type, even another
intersection type.

```js
type Foo = Type1 & Type2;
type Bar = Type3 & Type4;

type Baz = Foo & Bar;
```

## Intersection types require all in, but one out

Intersection types are the opposite of union types. When calling a function
that accepts an intersection type, we must pass in ***all of those types***. But
inside of our function we only have to treat it as ***any one of those
types***.

```js flow-check
// @flow
type A = { a: number };
type B = { b: boolean };
type C = { c: string };

function method(value: A & B & C) {
  var a: A = value;
  var b: B = value;
  var c: C = value;
}
```

Even as we treat our value as just one of the types, we do not get an error
because it satisfies all of them.

## Intersection of function types {#toc-intersection-of-function-types}

A common use of intersection types is to express functions that return
different results based on the input we pass in. Suppose for example
that we want to write the type of a function that
* returns a string, when we pass in the value `"string"`,
* returns a number, when we pass in the value `"number"`, and
* returns any possible type (`mixed`), when we pass in any other string.

The type of this function will be
```js
type Fn =
  & ((x: "string") => string)
  & ((x: "number") => number)
  & ((x: string) => null);
```
Each line in the above definition is called an *overload*, and we say that functions
of type `Fn` are *overloaded*.

Note the use of parentheses around the arrow types. These are necessary to override
the precedence of the "arrow" constructor over the intersection.

### Calling an overloaded function

Using the above definition we can declare a function `fn` that has the following behavior:
```js
declare var fn: Fn;
var n: string = fn("string"); // okay
var n: number = fn("number"); // okay
var n: boolean = fn("boolean"); // error: null is incompatible with number
```
Flow achieves this behavior by matching the type of the argument to the *first*
overload with a compatible parameter type. Notice for example that the argument
`"string"` matches both the first and the last overload. Flow will
just pick the first one. If no overload matches, Flow will raise an error at the
call site.

### Declaring overloaded functions

An equivalent way to declare the same function `fn` would be by using consecutive
"declare function" statements
```js
declare function fn(x: "string"): string;
declare function fn(x: "number"): number;
declare function fn(x: string): null;
```

A limitation in Flow is that it can't *check* the body of a function against
an intersection type. In other words, if we provided the following implementation
for `fn` right after the above declarations
```js
function fn(x) {
  if (x === "string") { return ""; }
  else if (x === "number") { return 0; }
  else { return null; }
}
```
Flow silently accepts it (and uses `Fn` as the inferred type), but does not check
the implementation against this signature. This makes this kind of declaration
a better suited candidate for library definitions, where implementations are omitted.


## Intersections of object types {#toc-intersections-of-object-types}

When you create an intersection of object types, you merge all of their
properties together.

For example, when you create an intersection of two objects with different sets
of properties, it will result in an object with all of the properties.

```js flow-check
// @flow
type One = { foo: number };
type Two = { bar: boolean };

type Both = One & Two;

var value: Both = {
  foo: 1,
  bar: true
};
```

When you have properties that overlap by having the same name, Flow follows the same
strategy as with overloaded functions: it will return the type of the first property
that matches this name.

For example, if you merge two objects with a property named `prop`, first with a
type of number and second with a type of boolean, accessing `prop` will return
`number`.

```js flow-check
type One = { prop: number };
type Two = { prop: boolean };

declare var both: One & Two;

var prop1: number = both.prop; // okay
var prop2: boolean = both.prop; // Error: number is incompatible with boolean
```


**Note:** When it comes to objects, the order-specific way in which intersection
types are implemented in Flow, may often seem counterintuitive from a set theoretic
point of view. In sets, the operands of intersection can change order arbitrarily
(commutative property). For this reason, it is a better practice to define this
kind of operation over object types using the *spread* operator, e.g. `{ ...One, ...Two }`,
where the ordering semantics are better specified.


## Impossible intersection types {#toc-impossible-intersection-types}

Using intersection types, it is possible to create types which are impossible
to create at runtime. Intersection types will allow you to combine any set of
types, even ones that conflict with one another.

For example, you can create an intersection of a number and a string.

```js flow-check
// @flow
type NumberAndString = number & string;

function method(value: NumberAndString) {
  // ...
}

// $ExpectError
method(3.14); // Error!
// $ExpectError
method('hi'); // Error!
```

But you can't possibly create a value which is both a *number and a string*,
but you can create a type for it. There's no practical use for creating types
like this, but it's a side effect of how intersection types work.
