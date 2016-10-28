/* @flow */
/*
---
id: objects
title: Objects
permalink: /docs/objects.html
prev: classes.html
next: functions.html
---
*/

/*
Objects can be created with object literals. The types of properties are fixed
based on their initializers.
*/

// @flow
var o = {
  x: 42,
  foo(x) { this.x = x; }
};
// $ExpectError
o.foo('hello');

/*
Flow infers the type of property `x` of the object to be number since it is
initialized with a `number`. The method call `foo()` on the object writes
`string` to that property. As expected, running Flow produces an error.

## Object Types

Object types are of the form:

```js
{ x1: T1; x2: T2; x3: T3;}
```

Here is an example of declaring an object type:
*/

// @flow
class Foo {}
var obj: {a: boolean; b: string; c: Foo} = {a: true, b: "Hi", c: new Foo()}

/*
Here is an example of Flow catching a problem with your object type:
*/

// @flow
class Bar {}
// $ExpectError
var badObj: {a: boolean; b: string; c: Foo} = {a: true, b: "Hi", c: new Bar()}

/*
## Reusable Object Types

Object types can be made reusable through the use of
[type aliases](type-aliases.html):
*/

// @flow
type MyType = {message: string; isAwesome: boolean};
function sayHello(data: MyType) {
  console.log(data.message);
}

var mySampleData: MyType = {message: 'Hello World', isAwesome: true};
sayHello(mySampleData);
sayHello({message: 'Hi', isAwesome: false});

/*
Object types can be added together with the intersection operator, `&`. See
[union and intersection types](http://flowtype.org/docs/union-intersection-types.html#_)
for details.

## Optional properties

Object types can have optional properties. The following code shows how
optional properties allow objects with missing properties to be typed.
*/

// @flow
var optObj: { a: string; b?: number } = { a: "hello" };

/*
When optional properties are accessed, Flow tracks the fact that they could
be `undefined`, and reports errors when they are used as is.
*/

// $ExpectError
optObj.b * 10 // error: undefined is incompatible with number

/*
One way to avoid errors is to dynamically check that an optional property exists
before using it. See [nullable types](http://flowtype.org/docs/nullable-types.html#_) for details.

## Property Variance

By default, objects are invariant with respect to their property types.
*/

function invariance(o: {p: ?number}) {
  let p = o.p;
  o.p = null;
  return p;
}
var subtype_p: {p: number} = {p: 0};
// $ExpectError
invariance(subtype_p);

/*
However, properties can be annotated as covariant:
*/

function covariance(o: {+p: ?number}) {}
covariance(subtype_p);

/*
Covariant properties can not be written:
*/

function covariance_err(o: {+p: ?number}) {
  // $ExpectError
  o.p = null;
}

/*
Or contravariant:
*/

function contravariance(o: {-p: number}) {
  o.p = 0;
}
var supertype_p: {p: ?number} = {p: null};
contravariance(supertype_p);

/*
Contravariant properties can not be read from:
*/

function contravariance_err(o: {-p: number}) {
  // $ExpectError
  return o.p;
}

/*
## Constructor Functions and Prototype Objects

Another way of creating objects in JavaScript is by using `new` on
constructor functions. A constructor function is typically an open method
that "initializes" some properties of `this`; and a `new` operation on such a
function calls it on a freshly created object before returning it.

Additionally, a constructor function may set various properties on its
`prototype` object. These properties are typically methods, and are inherited
by all objects created from that constructor function by a process known as
prototype chaining.
*/

// $WithLineNums
// @flow
function FuncBasedClass(x) { this.x = x; }
FuncBasedClass.prototype.f = function() { return this.x; }

var y = new FuncBasedClass(42);
var z: number = y.f();

/*
In this code, a `new` object is created by `new FuncBasedClass(42)`; this object
has a property `x` initialized by `FuncBasedClass` with the `number` passed to it.
The object also responds to the `f` method defined in `FuncBasedClass.prototype`,
so `y.f()` reads `y.x` and returns it. This fits with the expectation of a `number` as
expressed by the annotation at line 6, so this code typechecks.

Furthermore, Flow ensures that an object's type can always be viewed as a
subtype of its constructor's `prototype type`. (This is analogous to subtyping
based on class inheritance.) This means that the following code typechecks:
*/

var anObj: FuncBasedClass = new FuncBasedClass(42);

/*
## Adding properties

It is a common idiom in JavaScript to add properties to objects after they are
created. In fact, we have already seen this idiom on several occasions above:
when initializing properties of `this` properties in a constructor function;
when building a constructor function's `prototype` object; when building a
`module.exports` object; and so on.

Flow supports this idiom. As far as we know, this is a type system novelty:
supporting this idiom while balancing other constraints of the type system,
such as sound subtyping over objects and prototypes, can be quite tricky!

However, for a property that may be added to an object after its creation,
Flow cannot guarantee the existence of that property at a particular property
access operation; it can only check that its writes and reads are type-
consistent. Providing such guarantees for dynamic objects would significantly
complicate the analysis; this is a well-known fact (in technical terms, Flow's
analysis is heap-insensitive for strong updates).

For example, the following code typechecks:
*/

// @flow
function foo(p) { p.x = 42; }
function bar(q) { return q.f(); }

var o = { };
o.f = function() { return this.x; };

bar(o);
foo(o);

/*
In this code, when `bar(o)` is called, `o.x` is undefined; only later is it
initialized by `foo(o)`, but it is hard to track this fact statically.

Fortunately, though, the following code does not typecheck:
*/

// $ExpectError
var test: string = bar(o);

/*
In other words, Flow knows enough to infer that whenever the `x` property of
`o` does exist, it is a number, so a `string` should not be expected.

## Sealed object types

Unfortunately, supporting dynamically added properties means that Flow can miss
errors where the programmer accesses a non-existent property by mistake. Thus, Flow
also supports sealed object types, where accesses of non-existent properties are reported
as errors.

When object types appear as annotations, they are considered sealed. Also, non-empty
object literals are considered to have sealed object types. In fact, the only cases where
an object type is not sealed are when it describes an empty object literal (to be extended
by adding properties to it), an object literal with [spread properties](https://github.com/sebmarkbage/ecmascript-rest-spread), or when it describes a map (see below).

Overall, the weaker guarantee for dynamically added properties is a small cost
to pay for the huge increase in flexibility it affords. Specifically, it
allows Flow to usefully type check lots of idiomatic JavaScript code, while
trusting the programmer to follow the discipline of fully initializing an
object before making it available, which effectively ensures that any
dynamically added properties during initialization are only accessed after
initialization is complete.

In any case, for most objects you can altogether avoid adding properties
dynamically, in which case you get stronger guarantees. Furthermore, as
described above, object type annotations are sealed, so you can always force
sealing by going through an annotation (and sealing is enforced at module
boundaries).

## Objects as Maps

An object can be viewed as a map from `string` to some value type by setting and
getting its properties via bracket notation (i.e. dynamic accessors), instead of
dot notation. Flow infers a precise value type for the map: in other words, if
you only write `number` values to a map, you will read `number` values back
(rather than, say, `any`).

Such a map can be given a type of the form
*/

type MapOfNumbers = { [key: string]: number };
var numbers: MapOfNumbers = {
  ten: 10,
  twenty: 20,
};

/*
where `string` is the key type and `number` is the value type of the map.

### Maps as Records

Viewing an object as a map does not preclude viewing it as a record. However,
for such an object, the value type of the map does not interfere with the
types of the properties of the record. This is potentially unsound, but we
admit it because a sound design would necessarily lead to severe imprecision
in the types of properties.

### The `Object` type

This type describes "any object" and you can think of it like an
`any`-flavored version of an object type.

In JavaScript, everything is an object. Flow is a bit stricter and does not
consider primitive types to be subtypes of `Object`.)
*/

// $ExpectError
(0: Object);
// $ExpectError
("": Object);
// $ExpectError
(true: Object);
// $ExpectError
(null: Object);
// $ExpectError
(undefined: Object);

/*
Many other types can be treated as objects, however. Naturally objects are
compatible with `Object`, but so are functions and classes.
*/

({foo: "foo"}: Object);
(function() {}: Object);
(class {}: Object);
// $ExpectError
([]: Object); // Flow does not treat arrays as objects (likely to change)

/*
  ## Exact Object Types
  As we saw, the object type `{ x: string }` ensures that an object contains
  *at least* the property `x` of type `string`. However, `{ x: string }` may
  have other properties in addition to `x`.

  Sometimes we want to also make sure that `x` is the only property of the
  object. For this purpose there are exact object types, which use `{|` and `|}`
  instead of `{` and `}`:
*/

type User = { name: string, age: number };
type StrictUser = {| name: string, age: number |};

// Regular object types allow extra properties
({ name: "Foo", age: 27, foo: false }: User);
// Exact object types disallow extra properties
// $ExpectError
({ name: "Foo", age: 27, foo: false }: StrictUser);

// Otherwise, they behave similarly
({ name: "Foo", age: 27 }: User);
({ name: "Foo", age: 27 }: StrictUser);
// $ExpectError
({ name: "Foo" }: User);
// $ExpectError
({ name: "Foo" }: StrictUser); // Error: 'age' is missing

/*
 Exact object types are a very useful tool for helping Flow to refine unions of
 object types and notice typos on property names and refinements. Because
 `{ name: string }` only means "an object with *at least* a `name` property",
 Flow can't be sure that objects of that type don't also have *other*
 properties. For this reason, Flow won't error if it sees an access of a
 property called, say, `nname` because there's no guarantee that the object
 doesn't actually have a `nname` property on it!
*/
