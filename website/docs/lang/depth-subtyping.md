---
title: Depth Subtyping
slug: /lang/depth-subtyping
---

Assume we have two [classes](../../types/classes), which have a subtype relationship using `extends`:

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

```js
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

The plus sign `+` indicates that the `who` property is [covariant](../variance/#toc-covariance).
Using a covariant property allows us to use objects which have subtype-compatible
values for that property. By default, object properties are invariant, which allow
both reads and writes, but are more restrictive in the values they accept.

Read more about [property variance](../variance/).
