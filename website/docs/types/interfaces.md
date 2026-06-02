---
title: Interfaces
slug: /types/interfaces
description: "How to use interfaces in Flow for structural typing of classes and objects, including implementing and extending interfaces."
---

Interfaces declare the structure a [class](./classes.md) or object must satisfy without requiring a specific class identity. Because classes in Flow are [nominally typed](../lang/nominal-structural.md), two classes with identical members are not interchangeable — interfaces provide structural typing to bridge that gap.

```js flow-check
interface Serializable {
  serialize(): string;
}

class Foo {
  serialize(): string { return '[Foo]'; }
}

class Bar {
  serialize(): string { return '[Bar]'; }
}

const foo: Foo = new Bar(); // Error!

const foo2: Serializable = new Foo(); // Works!
const bar2: Serializable = new Bar(); // Works!
```

:::info TypeScript comparison
TypeScript types classes structurally, making interfaces and object types largely interchangeable. Flow diverges in three places: an [interface is not a subtype of an object type](../flow-vs-typescript.md#toc-classes-nominal) because interfaces may be backed by classes, which Flow types nominally; `implements` and `extends` [clauses must name an interface or class](../flow-vs-typescript.md#toc-implements-extends-rhs), not an arbitrary object type; and [primitives are not subtypes](../flow-vs-typescript.md#toc-primitives-interfaces) of interfaces or object types in Flow, while TS lets a `string` satisfy `{length: number}`.
:::

## When to use this {#toc-when-to-use}

Use interfaces over [object types](./objects.md) when you need to accept both class instances and plain objects with the same shape. Use interfaces over [classes](./classes.md) when you want structural compatibility — any value with matching properties is assignable, regardless of which class it was constructed from.

## Interfaces for instances and objects {#toc-interfaces-for-instances-and-objects}

Interfaces can describe both instances and objects, unlike object types which can only describe objects:

```js flow-check
class Foo {
  a: number;
}
const foo = new Foo();
const o: {a: number} = {a: 1};

interface MyInterface {
  a: number;
}

function acceptsMyInterface(x: MyInterface) { /* ... */ }
acceptsMyInterface(o); // Works!
acceptsMyInterface(foo); // Works!

function acceptsObj(x: {a: number, ...}) { /* ... */ }
acceptsObj(o); // Works!
acceptsObj(foo); // Error!
```

The underlying reason is [method unbinding](./classes.md#toc-method-unbinding): Flow tracks `this` on class methods, and letting a class instance flow into an object type would be a backdoor around that rule — extract `obj.method` through the object-type alias and call it without a receiver. Interfaces have the same hazard, so an interface-typed value also can't flow into an object type. Plain object literals carry no `this` binding to lose, which is why they flow into object types freely.

So three kinds of values relate to the two structural type shapes asymmetrically:

| value                  | assignable to object type | assignable to interface |
| ---------------------- | :-----------------------: | :---------------------: |
| object literal         | ✓                         | ✓                       |
| class instance         |                           | ✓                       |
| interface-typed value  |                           | ✓                       |

The error code depends on the object type's exactness: against an exact object type (the default) it's `[incompatible-exact]`; against an inexact object type (`{a: number, ...}`) it's `[class-object-subtyping]` with the suggestion to rewrite the object type as an interface. The fix in both cases is to switch the parameter type to an interface — interfaces accept all three kinds.

Unlike objects, interfaces cannot be [exact](./objects.md#exact-and-inexact-object-types), as they can always have other, unknown properties.

## Implementing interfaces {#toc-implementing-interfaces}

You can use `implements` to tell Flow that you want the class to match an
interface. This prevents you from making incompatible changes when editing the
class.

```js flow-check
interface Serializable {
  serialize(): string;
}

class Foo implements Serializable {
  serialize(): string { return '[Foo]'; } // Works!
}

class Bar implements Serializable {
  serialize(): number { return 42; } // Error!
}
```

You can also use `implements` with multiple interfaces.

```js
class Foo implements Bar, Baz {
  // ...
}
```

## Interface Syntax {#toc-interface-syntax}

Interfaces are created using the keyword `interface` followed by its name and
a block which contains the body of the type definition.

```js flow-check
interface MyInterface {
  // ...
}
```

The syntax of the block matches the syntax of object types.

### Anonymous interfaces {#toc-anonymous-interfaces}

You can also declare an anonymous interface inline:

```js flow-check
class Foo {
  a: number;
}

function getNumber(o: interface {a: number}): number {
  return o.a;
}

getNumber(new Foo()); // Works!
```

### Interface Methods {#toc-interface-methods}

You can add methods to interfaces following the same syntax as class methods. Any [`this` parameters](./functions.md#this-parameter) you
provide are also subject to the same restrictions as class methods.

```js flow-check
interface MyInterface {
  method(value: string): number;
}
```

Also like [class methods](./classes.md#toc-class-methods), interface methods must also remain bound to the interface on which they were defined.

You can define [overloaded methods](./intersections.md#declaring-overloaded-functions) by declaring the same method name multiple times with different type signatures:

```js flow-check
interface MyInterface {
  method(value: string): string;
  method(value: boolean): boolean;
}

function func(a: MyInterface) {
  const x: string = a.method('hi'); // Works!
  const y: boolean = a.method(true); // Works!

  const z: boolean = a.method('hi'); // Error!
}
```

### Interface Properties {#toc-interface-properties}

You can add properties to interfaces following the same syntax as class
properties:

```js flow-check
interface MyInterface {
  property: string;
}
```

Interface properties can be optional as well:

```js flow-check
interface MyInterface {
  property?: string;
}
```

### Interfaces as maps {#toc-interfaces-as-maps}

You can create [indexer properties](./objects.md#toc-objects-as-maps) the same
way as with objects:

```js flow-check
interface MyInterface {
  [key: string]: number;
}
```

### Interface Generics {#toc-interface-generics}

Interfaces can also have their own [generics](./generics.md):

```js flow-check
interface MyInterface<A, B, C> {
  property: A;
  method(val: B): C;
}
```

Interface generics are [parameterized](./generics.md#toc-parameterized-generics).
When you use an interface you need to pass parameters for each of its generics:

```js flow-check
interface MyInterface<A, B, C> {
  foo: A;
  bar: B;
  baz: C;
}

const val: MyInterface<number, boolean, string> = {
  foo: 1,
  bar: true,
  baz: 'three',
};
```

## Primitives are not subtypes of interfaces {#toc-primitives-not-subtypes}

Flow rejects primitives (`string`, `number`, `boolean`) flowing into an interface or object type, even when their boxed prototype would satisfy the contract. The hazard is silent shape ambiguity: a bare `string` structurally satisfies `Iterable<string>` (via `String.prototype[Symbol.iterator]`), but iterating it yields code points instead of items — almost always a logic bug rather than the intent.

```js flow-check
function logAll(items: Iterable<string>) {
  for (const x of items) console.log(x);
}

const msgs = "foo";
logAll(msgs); // Error: `string` is not a subtype of `Iterable<string>`
```

If a one-element iterable was intended, wrap the value with `[s]` at the call site; otherwise the call was passing the wrong shape.

## Extending interfaces {#toc-extending-interfaces}

An interface can extend other interfaces or classes to inherit their members:

```js flow-check
interface Animal {
  name: string;
}
interface Dog extends Animal {
  breed: string;
}

const d: Dog = {name: "Rex", breed: "Lab"};
const a: Animal = d; // Works!
```

The right-hand side of `extends` must name an interface or class — passing an object-type alias errors with `[incompatible-use]` ("not inheritable"). The same constraint applies to the `implements` clause on a class (`[cannot-implement]`). Mapped or utility types applied to interfaces produce object types, so they also won't work in either position. The rewrite is to introduce a named interface (or inline the members directly) instead.

## Interface property variance (read-only and write-only) {#toc-interface-property-variance-read-only-and-write-only}

Interface properties are [invariant](../lang/variance.md) by default. But you
can add modifiers to make them covariant (read-only) or contravariant
(write-only).

```js flow-check
interface MyInterface {
  readonly covariant: number;      // read-only
  writeonly contravariant: number; // write-only
}
```

### Covariant (read-only) properties on interfaces {#toc-covariant-read-only-properties-on-interfaces}

You can make a property covariant by adding the `readonly` keyword in front of the
property name:

```js flow-check
interface MyInterface {
  readonly readOnly: number | string;
}
```

This allows you to pass a more specific type in place of that property:

```js flow-check
interface Invariant {
  property: number | string;
}
interface Covariant {
  readonly readOnly: number | string;
}

const x: {property: number} = {property: 42};
const y: {readOnly: number} = {readOnly: 42};

const value1: Invariant = x; // Error!
const value2: Covariant = y; // Works
```

Because of how covariance works, covariant properties also become read-only
when used. Which can be useful over normal properties.

```js flow-check
interface Invariant {
  property: number | string;
}
interface Covariant {
  readonly readOnly: number | string;
}

function func1(value: Invariant) {
  value.property;        // Works!
  value.property = 3.14; // Works!
}

function func2(value: Covariant) {
  value.readOnly;        // Works!
  value.readOnly = 3.14; // Error!
}
```

### Contravariant (write-only) properties on interfaces {#toc-contravariant-write-only-properties-on-interfaces}

You can make a property contravariant by adding the `writeonly` keyword in front of
the property name.

```js flow-check
interface InterfaceName {
  writeonly writeOnly: number;
}
```

This allows you to pass a less specific type in place of that property.

```js flow-check
interface Invariant {
  property: number;
}
interface Contravariant {
  writeonly writeOnly: number;
}

const numberOrString = Math.random() > 0.5 ? 42 : 'forty-two';

const value1: Invariant     = {property: numberOrString};  // Error!
const value2: Contravariant = {writeOnly: numberOrString}; // Works!
```

Because of how contravariance works, contravariant properties also become
write-only when used. Which can be useful over normal properties.

```js flow-check
interface Invariant {
  property: number;
}
interface Contravariant {
  writeonly writeOnly: number;
}

function func1(value: Invariant) {
  value.property;        // Works!
  value.property = 3.14; // Works!
}

function func2(value: Contravariant) {
  value.writeOnly;        // Error!
  value.writeOnly = 3.14; // Works!
}
```

## See Also {#toc-see-also}

- [Classes](./classes.md) — nominally typed values that can implement interfaces
- [Objects](./objects.md) — structurally typed object types, which cannot describe class instances
- [Nominal & Structural Typing](../lang/nominal-structural.md) — the difference between name-based and shape-based typing
- [Variance](../lang/variance.md) — covariant (read-only) and contravariant (write-only) properties
