---
title: Object Subtyping
slug: /lang/object-subtyping
description: "How Flow handles subtyping for objects, including depth subtyping with read-only properties and width subtyping."
---

## Depth Subtyping {#toc-depth-subtyping}

Assume we have two [classes](../types/classes.md), which have a subtype relationship using `extends`:

```js flow-check
class Person {
  name: string;
}
class Employee extends Person {
  department: string;
}
```

It's valid to use an `Employee` instance where a `Person` instance is expected.

```js flow-check
class Person { name: string }
class Employee extends Person { department: string }

const employee: Employee = new Employee();
const person: Person = employee; // OK
```

However, it is not valid to use an object containing an `Employee` instance
where an object containing a `Person` instance is expected.

```js flow-check
class Person { name: string }
class Employee extends Person { department: string }

const employee: {who: Employee} = {who: new Employee()};
const person: {who: Person} = employee; // Error
```

This is an error because objects are mutable. The value referenced by the
`employee` variable is the same as the value referenced by the `person`
variable.

```js flow-check
class Person { name: string }

declare const person: {who: Person};
person.who = new Person();
```

If we write into the `who` property of the `person` object, we've also changed
the value of `employee.who`, which is explicitly annotated to be an `Employee`
instance.

If we prevented any code from ever writing a new value to the object through
the `person` variable, it would be safe to use the `employee` variable. Flow
provides a syntax for this:

```js flow-check
class Person { name: string }
class Employee extends Person { department: string }

const employee: {who: Employee} = {who: new Employee()};
const person: {+who: Person} = employee; // OK
person.who = new Person(); // Error!
```

The plus sign `+` indicates that the `who` property is [covariant](./variance.md#toc-covariance).
Using a covariant property allows us to use objects which have subtype-compatible
values for that property. By default, object properties are invariant, which allow
both reads and writes, but are more restrictive in the values they accept.

Read more about [property variance](./variance.md).

## Width Subtyping {#toc-width-subtyping}

It's safe to use an object with "extra" properties in a position that is
annotated with a specific set of properties, if that object type is [inexact](../types/objects.md#exact-and-inexact-object-types).

```js flow-check
function func(obj: {foo: string, ...}) {
  // ...
}

func({
  foo: "test", // Works!
  bar: 42      // Works!
});
```

Within `func`, we know that `obj` has at least a property `foo` and the
property access expression `obj.foo` will have type `string`.

This is a kind of subtyping commonly referred to as "width subtyping" because
a type that is "wider" (i.e., has more properties) is a subtype of a
narrower type.

So in the following example, `obj2` is a _subtype_ of `obj1`.

```js flow-check
let obj1: {foo: string, ...}  = {foo: 'test'};
let obj2 = {foo: 'test', bar: 42};
obj2 as {foo: string, ...};
```

However, it's often useful to know that a property is definitely absent.

```js flow-check
function func(obj: {foo: string, ...} | {bar: number, ...}) {
  if (obj.foo) {
    obj.foo as string; // Error!
  }
}
```

The above code has a type error because Flow would also allow the call
expression `func({foo: 1, bar: 2})`, because `{foo: number, bar: number}`
is a subtype of `{bar: number, ...}`, one of the members of the parameter's union
type.

For cases like this where it's useful to assert the absence of a property,
You can use [exact object types](../types/objects.md#exact-and-inexact-object-types).

```js flow-check
function func(obj: {foo: string} | {bar: number}) {
  if (obj.foo) {
    obj.foo as string; // Works!
  }
}
```

[Exact object types](../types/objects.md#exact-and-inexact-object-types) disable width
subtyping, and do not allow additional properties to exist.

Using exact object types lets Flow know that no extra properties will exist at
runtime, which allows [refinements](./refinements.md) to get more specific.

## See Also {#toc-see-also}

- [Variance](./variance.md) — how covariance enables depth subtyping with read-only properties
- [Subtypes](./subtypes.md) — the general theory of subtyping
- [Objects](../types/objects.md) — read-only and write-only object properties
