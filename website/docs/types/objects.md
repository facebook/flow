---
title: Object Types
slug: /types/objects
---

Objects can be used in many different ways in JavaScript. There are a number of
different ways to type them in order to support all the different use cases.

In Flow, there are two different kinds of object types: exact object types and inexact object types.

In general, we recommend using [exact object types](#toc-exact-object-types) whenever possible. Exact object types are more
precise and interact better with other type system features, like spreads.

## Object type syntax {#toc-object-type-syntax}

Object types try to match the syntax for objects in JavaScript as much as
possible. Using curly braces `{}` and name-value pairs using a colon `:` split
by commas `,`.

```js flow-check
// @flow
var obj1: { foo: boolean } = { foo: true };
var obj2: {
  foo: number,
  bar: boolean,
  baz: string,
} = {
  foo: 1,
  bar: true,
  baz: 'three',
};
```

> **Note:** Previously object types used semicolons `;` for splitting
> name-value pairs. While the syntax is still valid, you should use commas `,`.

### Optional object type properties {#toc-optional-object-type-properties}

In JavaScript, accessing a property that doesn't exist evaluates to
`undefined`. This is a common source of errors in JavaScript programs, so Flow
turns these into type errors.

```js flow-check
// @flow
var obj = { foo: "bar" };
// $ExpectError
obj.bar; // Error!
```

If you have an object that sometimes does not have a property you can make it
an _optional property_ by adding a question mark `?` after the property name in
the object type.

```js flow-check
// @flow
var obj: { foo?: boolean } = {};

obj.foo = true;    // Works!
// $ExpectError
obj.foo = 'hello'; // Error!
```

In addition to their set value type, these optional properties can either be
`void` or omitted altogether. However, they cannot be `null`.

```js flow-check
// @flow
function acceptsObject(value: { foo?: string }) {
  // ...
}

acceptsObject({ foo: "bar" });     // Works!
acceptsObject({ foo: undefined }); // Works!
// $ExpectError
acceptsObject({ foo: null });      // Error!
acceptsObject({});                 // Works!
```

### Object methods {#toc-object-methods}

Method syntax in objects has the same runtime behavior as a function property. These two objects are equivalent at runtime:

```js flow-check
// @flow
let a = {
  foo : function () { return 3; }
};
let b = {
  foo() { return 3; }
}
```

However, despite their equivalent runtime behavior, Flow checks them slightly differently. In particular, object
properties written with method syntax are read-only; Flow will not allow you to write a new value to them.

```js flow-check
// @flow
let b = {
  foo() { return 3; }
}
b.foo = () => { return 2; } // Error!
```

Additionally, object methods do not allow the use of `this` in their bodies, in order to guarantee simple behavior
for their `this` parameters. Prefer to reference the object by name instead of using `this`.

```js flow-check
// @flow
let a = {
  x : 3,
  foo() { return this.x; } // error!
}
let b = {
  x : 3,
  foo(): number { return b.x; } // works!
}
```

## Object type inference {#toc-object-type-inference}

> NOTE: The behavior of empty object literals has changed as of version 0.191 -
> see this [blog post](https://medium.com/flow-type/improved-handling-of-the-empty-object-in-flow-ead91887e40c) for more details.

When you create an object value, its type is set at the creation point. You cannot add new properties,
or modify the type of existing properties.

```js flow-check
const obj = {
  foo: 1,
  bar: true,
};

const n: number  = obj.foo; // Works!
const b: boolean = obj.bar; // Works!

obj.UNKNOWN; // Error - prop `UNKNOWN` is not in the object value
obj.foo = true; // Error - `foo` is of type `number`
```

If you supply an annotation, you can add properties missing in the object value as optional properties:

```js flow-check
const obj: {
  foo?: number,
  bar: boolean,
} = {
  // `foo` is not set here
  bar: true,
};

const n: number | void = obj.foo; // Works!
const b: boolean = obj.bar; // Works!

if (b) {
  obj.foo = 3; // Works!
}
```

You can also give a wider type for a particular property:

```js flow-check
const obj: {
  foo: number | string,
} = {
  foo: 1,
};

const foo: number | string = obj.foo; // Works!
obj.foo = "hi"; // Works!
```

The empty object can be interpreted as a [dictionary](#toc-objects-as-maps), if you supply the appropriate annotation:

```js flow-check
const dict: {[string]: number} = {}; // Works!
```

## Exact and inexact object types {#toc-exact-object-types}

Eact object types are the default (as of version 0.202), unless you have set [`exact_by_default=false`](../../config/options#toc-exact-by-default-boolean) in your `.flowconfig`.

Inexact objects (denoted with the `...`) allow extra properties to be passed in:

```js flow-check
function method(obj: {foo: string, ...}) {
  // ...
}

method({foo: "test", bar: 42}); // Works!
```

> **Note:** This is because of ["width subtyping"](../../lang/width-subtyping).

But exact object types do not:

```js flow-check
function method(obj: {foo: string}) {
  // ...
}

method({foo: "test", bar: 42}); // Error!
```

If you have set `exact_by_default=false`, you can denote exact object types by adding a pair of "vertical bars" or "pipes" to the inside of the curly braces:

```js flow-check
const x: {|foo: string|} = {foo: "Hello", bar: "World!"}; // Error!
```

Intersections of exact object types may not work as you expect. If you need to combine exact object types, use object type spread:

```js flow-check
type FooT = {foo: string};
type BarT = {bar: number};

type FooBarFailT = FooT & BarT;
type FooBarT = {...FooT, ...BarT};

const fooBarFail: FooBarFailT = {foo: '123', bar: 12}; // Error!
const fooBar: FooBarT = {foo: '123', bar: 12}; // Works!
```

## Objects as maps {#toc-objects-as-maps}

Newer versions of the JavaScript standard include a `Map` class, but it is
still very common to use objects as maps as well. In this use case, an object
will likely have properties added to it and retrieved throughout its lifecycle.
Furthermore, the property keys may not even be known statically, so writing out
a type annotation would not be possible.

For objects like these, Flow provides a special kind of property, called an
"indexer property." An indexer property allows reads and writes using any key
that matches the indexer key type.

```js flow-check
// @flow
var o: { [string]: number } = {};
o["foo"] = 0;
o["bar"] = 1;
var foo: number = o["foo"];
```

An indexer can be optionally named, for documentation purposes:

```js flow-check
// @flow
var obj: { [user_id: number]: string } = {};
obj[1] = "Julia";
obj[2] = "Camille";
obj[3] = "Justin";
obj[4] = "Mark";
```

When an object type has an indexer property, property accesses are assumed to
have the annotated type, even if the object does not have a value in that slot
at runtime. It is the programmer's responsibility to ensure the access is safe,
as with arrays.

```js flow-check
var obj: { [number]: string } = {};
obj[42].length; // No type error, but will throw at runtime
```

Indexer properties can be mixed with named properties:

```js flow-check
// @flow
var obj: {
  size: number,
  [id: number]: string
} = {
  size: 0
};

function add(id: number, name: string) {
  obj[id] = name;
  obj.size++;
}
```

### `Object` Type {#toc-object-type}

> NOTE: For new code, prefer `any` or `{ [key: string]: any}`. `Object` is an alias to [`any`](../any) and will
> be deprecated and removed in a future version of Flow.

Sometimes it is useful to write types that accept arbitrary objects, for
those you should write `{...}` like this:

```js
function method(obj: {...}) {
  // ...
}
```

However, if you need to opt-out of the type checker, and don't want to go all
the way to `any`, you could use `{ [key: string]: any}`. (Note that [`any`](../any) is unsafe and
should be avoided). For historical reasons, the `Object` keyword is still available.
In previous versions of Flow, `Object` was the same
as `{ [key: string]: any}`.

For example, the following code will not report any errors:

```js
function method(obj: { [key: string]: any }) {
  obj.foo = 42;               // Works.
  let bar: boolean = obj.bar; // Works.
  obj.baz.bat.bam.bop;        // Works.
}

method({ baz: 3.14, bar: "hello" });
```

Neither will this:

```js
function method(obj: Object) {
  obj = 10;
}

method({ baz: 3.14, bar: "hello" });
```
