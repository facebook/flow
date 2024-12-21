---
title: Intersections
slug: /types/intersections
---

Sometimes it is useful to create a type which is ***all of*** a set of other
types. For example, you might want to write a function which accepts a value that
implements two different [interfaces](../interfaces):

```js flow-check
interface Serializable {
  serialize(): string;
}

interface HasLength {
  length: number;
}

function func(value: Serializable & HasLength) {
  // ...
}

func({
  length: 3,
  serialize() {
    return '3';
  },
}); // Works

func({length: 3}); // Error! Doesn't implement both interfaces
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
type A = {a: number, ...};
type B = {b: boolean, ...};
type C = {c: string, ...};

function func(value: A & B & C) {
  const a: A = value;
  const b: B = value;
  const c: C = value;
}
```

Even as we treat our value as just one of the types, we do not get an error
because it satisfies all of them.

## Intersection of function types {#toc-intersection-of-function-types}

A common use of intersection types is to express functions that return
different results based on the input we pass in. Suppose for example
that we want to write the type of a function that:
* returns a string, when we pass in the value `"string"`,
* returns a number, when we pass in the value `"number"`, and
* returns any possible type (`mixed`), when we pass in any other string.

The type of this function will be
```js flow-check
type Fn =
  & ((x: "string") => string)
  & ((x: "number") => number)
  & ((x: string) => mixed);
```
Each line in the above definition is called an *overload*, and we say that functions
of type `Fn` are *overloaded*.

Note the use of parentheses around the arrow types. These are necessary to override
the precedence of the "arrow" constructor over the intersection.

### Calling an overloaded function

Using the above definition we can declare a function `fn` that has the following behavior:
```js flow-check
declare const fn:
  & ((x: "string") => string)
  & ((x: "number") => number)
  & ((x: string) => mixed);

const s: string = fn("string"); // Works
const n: number = fn("number"); // Works
const b: boolean = fn("boolean"); // Error!
```
Flow achieves this behavior by matching the type of the argument to the *first*
overload with a compatible parameter type. Notice for example that the argument
`"string"` matches both the first and the last overload. Flow will
just pick the first one. If no overload matches, Flow will raise an error at the
call site.

### Declaring overloaded functions

An equivalent way to declare the same function `fn` would be by using consecutive
"declare function" statements
```js flow-check
declare function fn(x: "string"): string;
declare function fn(x: "number"): number;
declare function fn(x: string): mixed;
```

A limitation in Flow is that it can't *check* the body of a function against
an intersection type. In other words, if we provided the following implementation
for `fn` right after the above declarations
```js flow-check
function fn(x: mixed) {
  if (x === "string") { return ""; }
  else if (x === "number") { return 0; }
  else { return null; }
}
```
Flow silently accepts it (and uses `Fn` as the inferred type), but does not check
the implementation against this signature. This makes this kind of declaration
a better suited candidate for [library definitions](../../libdefs/), where implementations are omitted.


## Intersections of object types {#toc-intersections-of-object-types}

When you create an intersection of [inexact object types](../objects/#exact-and-inexact-object-types),
you are saying that your object satisfies each member of the intersection.

For example, when you create an intersection of two inexact objects with different sets
of properties, it will result in an object with all of the properties.

```js flow-check
type One = {foo: number, ...};
type Two = {bar: boolean, ...};

type Both = One & Two;

const value: Both = {
  foo: 1,
  bar: true
};
```

When you have properties that overlap by having the same name, Flow follows the same
strategy as with overloaded functions: it will return the type of the first property
that matches this name.

For example, if you merge two inexact objects with a property named `prop`, first with a
type of `number` and second with a type of `boolean`, accessing `prop` will return
`number`.

```js flow-check
type One = {prop: number, ...};
type Two = {prop: boolean, ...};

declare const both: One & Two;

const prop1: number = both.prop; // Works
const prop2: boolean = both.prop; // Error!
```

To combine exact object types, you should use [object type spread](../objects/#object-type-spread) instead:

```js flow-check
type One = {foo: number};
type Two = {bar: boolean};

type Both = {
  ...One,
  ...Two,
};

const value: Both = {
  foo: 1,
  bar: true
};
```

**Note:** When it comes to objects, the order-specific way in which intersection
types are implemented in Flow, may often seem counter-intuitive from a set theoretic
point of view. In sets, the operands of intersection can change order arbitrarily
(commutative property). For this reason, it is a better practice to define this
kind of operation over object types using object type spread where the ordering
semantics are better specified.


## Impossible intersection types {#toc-impossible-intersection-types}

Using intersection types, it is possible to create types which are impossible
to create at runtime. Intersection types will allow you to combine any set of
types, even ones that conflict with one another.

For example, you can create an intersection of a number and a string.

```js flow-check
type NumberAndString = number & string;

function func(value: NumberAndString) { /* ... */ }

func(3.14); // Error!
func('hi'); // Error!
```

But you can't possibly create a value which is both a *number and a string*,
but you can create a type for it. There's no practical use for creating types
like this, but it's a side effect of how intersection types work.

An accidental way to create an impossible type is to create an intersection of
[exact object types](../objects/#exact-and-inexact-object-types). For example:

```js flow-check
function func(obj: {a: number} & {b: string}) { /* ... */ }

func({a: 1}); // Error!
func({b: 'hi'}); // Error!
func({a: 1, b: 'hi'}); // Error!
```

It's not possible for an object to have exactly the property `a` and no other
properties, and simultaneously exactly the property `b` and no other properties.
