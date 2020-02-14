---
layout: guide
---

Objects can be used in many different ways in JavaScript. There are a number of
different ways to type them in order to support all the different use cases.

In Flow, there are two different kinds of object types: exact object types and inexact object types.

In general, we recommend using [exact object types](#toc-exact-object-types) whenever possible. Exact object types are more
precise and interact better with other type system features, like spreads.

## Object type syntax <a class="toc" id="toc-object-type-syntax" href="#toc-object-type-syntax"></a>

Object types try to match the syntax for objects in JavaScript as much as
possible. Using curly braces `{}` and name-value pairs using a colon `:` split
by commas `,`.

```js
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

#### Optional object type properties <a class="toc" id="toc-optional-object-type-properties" href="#toc-optional-object-type-properties"></a>

In JavaScript, accessing a property that doesn't exist evaluates to
`undefined`. This is a common source of errors in JavaScript programs, so Flow
turns these into type errors.

```js
// @flow
var obj = { foo: "bar" };
// $ExpectError
obj.bar; // Error!
```

If you have an object that sometimes does not have a property you can make it
an _optional property_ by adding a question mark `?` after the property name in
the object type.

```js
// @flow
var obj: { foo?: boolean } = {};

obj.foo = true;    // Works!
// $ExpectError
obj.foo = 'hello'; // Error!
```

In addition to their set value type, these optional properties can either be
`void` or omitted altogether. However, they cannot be `null`.

```js
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

## Object type inference <a class="toc" id="toc-object-type-inference" href="#toc-object-type-inference"></a>

Flow can infer the type of object literals in two different ways depending on
how they are used.

### Sealed objects <a class="toc" id="toc-sealed-objects" href="#toc-sealed-objects"></a>

When you create an object with its properties, you create a _sealed_ object
type in Flow. These sealed objects will know all of the properties you declared
them with and the types of their values.

```js
// @flow
var obj = {
  foo: 1,
  bar: true,
  baz: 'three'
};

var foo: number  = obj.foo; // Works!
var bar: boolean = obj.bar; // Works!
// $ExpectError
var baz: null    = obj.baz; // Error!
var bat: string  = obj.bat; // Error!
```

But when objects are sealed, Flow will not allow you to add new properties to
them.

```js
// @flow
var obj = {
  foo: 1
};

// $ExpectError
obj.bar = true;    // Error!
// $ExpectError
obj.baz = 'three'; // Error!
```

The workaround here might be to turn your object into an _unsealed object_.

### Unsealed objects <a class="toc" id="toc-unsealed-objects" href="#toc-unsealed-objects"></a>

When you create an object without any properties, you create an _unsealed_
object type in Flow. These unsealed objects will not know all of their
properties and will allow you to add new ones.

```js
// @flow
var obj = {};

obj.foo = 1;       // Works!
obj.bar = true;    // Works!
obj.baz = 'three'; // Works!
```

The inferred type of the property becomes what you set it to.

```js
// @flow
var obj = {};
obj.foo = 42;
var num: number = obj.foo;
```

##### Reassigning unsealed object properties <a class="toc" id="toc-reassigning-unsealed-object-properties" href="#toc-reassigning-unsealed-object-properties"></a>

Similar to [`var` and `let` variables](../variables/#toc-reassigning-variables)
if you reassign a property of an unsealed object, by default Flow will give it
the type of all possible assignments.

```js
// @flow
var obj = {};

if (Math.random()) obj.prop = true;
else obj.prop = "hello";

// $ExpectError
var val1: boolean = obj.prop; // Error!
// $ExpectError
var val2: string  = obj.prop; // Error!
var val3: boolean | string = obj.prop; // Works!
```

Sometimes Flow is able to figure out (with certainty) the type of a property
after reassignment. In that case, Flow will give it the known type.

```js
// @flow
var obj = {};

obj.prop = true;
obj.prop = "hello";

// $ExpectError
var val1: boolean = obj.prop; // Error!
var val2: string  = obj.prop; // Works!
```

As Flow gets smarter and smarter, it will figure out the types of properties in more scenarios.

##### Unknown property lookup on unsealed objects is unsafe <a class="toc" id="toc-unknown-property-lookup-on-unsealed-objects-is-unsafe" href="#toc-unknown-property-lookup-on-unsealed-objects-is-unsafe"></a>

Unsealed objects allow new properties to be written at any time. Flow ensures
that reads are compatible with writes, but does not ensure that writes happen
before reads (in the order of execution).

This means that reads from unsealed objects with no matching writes are never
checked. This is an unsafe behavior of Flow which may be improved in the
future.

```js
var obj = {};

obj.foo = 1;
obj.bar = true;

var foo: number  = obj.foo; // Works!
var bar: boolean = obj.bar; // Works!
var baz: string  = obj.baz; // Works?
```

## Exact object types <a class="toc" id="toc-exact-object-types" href="#toc-exact-object-types"></a>

In Flow, it is considered safe to pass an object with extra properties where
a normal object type is expected.

```js
// @flow
function method(obj: { foo: string }) {
  // ...
}

method({
  foo: "test", // Works!
  bar: 42      // Works!
});
```

> **Note:** This is because of ["width subtyping"](../../lang/width-subtyping/).

Sometimes it is useful to disable this behavior and only allow a specific set
of properties. For this, Flow supports "exact" object types.

```js
{| foo: string, bar: number |}
```

Unlike regular object types, it is not valid to pass an object with "extra"
properties to an exact object type.

```js
// @flow
var foo: {| foo: string |} = { foo: "Hello", bar: "World!" }; // Error!
```

Intersections of exact object types may not work as you expect. If you need to combine exact object types, use object type spread:

```js
// @flow

type FooT = {| foo: string |};
type BarT = {| bar: number |};

type FooBarFailT = FooT & BarT;
type FooBarT = {| ...FooT, ...BarT |};

const fooBarFail: FooBarFailT = { foo: '123', bar: 12 }; // Error!
const fooBar: FooBarT = { foo: '123', bar: 12 }; // Works!
```

## Explicit inexact object types <a class="toc" id="toc-explicit-inexact-object-types" href="#toc-explicit-inexact-object-types"></a>

In addition to the default `{}` syntax, you can explicitly indicate an inexact
object by using an ellipsis at the end of your property list:

```js
// @flow

type Inexact = {foo: number, ...};
```

[Flow is planning to make object types exact by default](https://medium.com/flow-type/on-the-roadmap-exact-objects-by-default-16b72933c5cf).
This is available via an [option in your flowconfig](../../config/options/#toc-exact-by-default-boolean).
You can also read our [upgrade guide](https://medium.com/flow-type/how-to-upgrade-to-exact-by-default-object-type-syntax-7aa44b4d08ab)
for steps to enable this option in your own project.

In a project using exact-by-default syntax, the explicit inexact object type syntax is the only way to express an inexact object type.

## Objects as maps <a class="toc" id="toc-objects-as-maps" href="#toc-objects-as-maps"></a>

Newer versions of the JavaScript standard include a `Map` class, but it is
still very common to use objects as maps as well. In this use case, an object
will likely have properties added to it and retrieved throughout its lifecycle.
Furthermore, the property keys may not even be known statically, so writing out
a type annotation would not be possible.

For objects like these, Flow provides a special kind of property, called an
"indexer property." An indexer property allows reads and writes using any key
that matches the indexer key type.

```js
// @flow
var o: { [string]: number } = {};
o["foo"] = 0;
o["bar"] = 1;
var foo: number = o["foo"];
```

An indexer can be optionally named, for documentation purposes:

```js
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

```js
var obj: { [number]: string } = {};
obj[42].length; // No type error, but will throw at runtime
```

Indexer properties can be mixed with named properties:

```js
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

### `Object` Type <a class="toc" id="toc-object-type" href="#toc-object-type"></a>

> NOTE: For new code, prefer `any` or `{ [key: string]: any}`. `Object` is an alias to [`any`](../any/) and will
> be deprecated and removed in a future version of Flow. 

Sometimes it is useful to write types that accept arbitrary objects, for
those you should write `{}` like this:

```js
function method(obj: {}) {
  // ...
}
```

However, if you need to opt-out of the type checker, and don't want to go all
the way to `any`, you could use `{ [key: string]: any}`. (Note that [`any`](../any/) is unsafe and
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
