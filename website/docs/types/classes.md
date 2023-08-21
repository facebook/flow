---
title: Classes
slug: /types/classes
---

JavaScript [classes](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes)
in Flow operate both as a value and a type. You can use the name of the class as the type of its instances:

```js flow-check
class MyClass {
  // ...
}

const myInstance: MyClass = new MyClass(); // Works!
```

This is because classes in Flow are [nominally typed](../../lang/nominal-structural).

This means two classes with identical shapes are not compatible:

```js flow-check
class A {
  x: number;
}
class B {
  x: number;
}
const foo: B = new A(); // Error!
const bar: A = new B(); // Error!
```

You also cannot use an [object type](../objects) to describe an instance of a class:

```js flow-check
class MyClass {
  x: number;
}
const foo: {x: number, ...} = new MyClass(); // Error!
```

You can use [interfaces](../interfaces) to accomplish this instead:

```js flow-check
class A {
  x: number;
}
class B {
  x: number;
}

interface WithXNum {
  x: number;
}

const foo: WithXNum = new A(); // Works!
const bar: WithXNum = new B(); // Works!

const n: number = foo.x; // Works!
```

## Class Syntax {#toc-class-syntax}

Classes in Flow are just like normal JavaScript classes, but with added types.

### Class Methods {#toc-class-methods}

Just like in [functions](../functions), class methods can have annotations for both parameters
(input) and returns (output):

```js flow-check
class MyClass {
  method(value: string): number {
    return 0;
  }
}
```

Also just like regular functions, class methods may have `this` annotations as well.
However, if one is not provided, Flow will infer the class instance type (or the class type for static methods)
instead of `mixed`. When an explicit `this` parameter is provided, it must be a [supertype](../../lang/subtypes/) of
the class instance type (or class type for static methods).

```js flow-check
class MyClass {
  method(this: interface {x: string}) { /* ... */ } // Error!
}
```

Unlike class properties, however, class methods cannot be unbound or rebound from
the class on which you defined them. So all of the following are errors in Flow:

```js flow-check
class MyClass { method() {} }
const a = new MyClass();
a.method; // Error!
const {method} = a; // Error!
a.method.bind({}); // Error!
```

Methods are considered [read-only](../../lang/variance):

```js flow-check
class MyClass {
  method() {}
}

const a = new MyClass();
a.method = function() {}; // Error!
```

Flow supports [private methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Private_class_fields),
a feature of ES2022. Private methods start with a hash symbol `#`:

```js flow-check
class MyClass {
  #internalMethod() {
    return 1;
  }
  publicApi() {
    return this.#internalMethod();
  }
}

const a = new MyClass();
a.#internalMethod(); // Error!
a.publicApi(); // Works!
```

Flow requires return type annotations on methods in most cases.
This is because it is common to reference `this` inside of a method, and `this` is typed as the instance of the class -
but to know the type of the class we need to know the return type of its methods!

```js flow-check
class MyClass {
  foo() { // Error!
    return this.bar();
  }
  bar() { // Error!
    return 1;
  }
}
```
```js flow-check
class MyClassFixed {
  foo(): number { // Works!
    return this.bar();
  }
  bar(): number { // Works!
    return 1;
  }
}
```

### Class Fields (Properties) {#toc-class-fields-properties}

Whenever you want to use a class field in Flow you must first give it an
annotation:

```js flow-check
class MyClass {
  method() {
    this.prop = 42; // Error!
  }
}
```

Fields are annotated within the body of the class with the field name followed
by a colon `:` and the type:

```js flow-check
class MyClass {
  prop: number;
  method() {
    this.prop = 42;
  }
}
```

Fields added outside of the class definition need to be annotated within the body
of the class:

```js flow-check
function func(x: number): number {
  return x + 1;
}

class MyClass {
  static constant: number;
  static helper: (number) => number;
  prop: number => number;
}
MyClass.helper = func
MyClass.constant = 42
MyClass.prototype.prop = func
```

Flow also supports using the [class properties syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes#field_declarations):

```js flow-check
class MyClass {
  prop = 42;
}
```

When using this syntax, you are not required to give it a type annotation. But
you still can if you need to:

```js flow-check
class MyClass {
  prop: number = 42;
}
```

You can mark a class field as read-only (or write-only) using [variance](../../lang/variance) annotations.
These can only be written to in the constructor:

```js flow-check
class MyClass {
  +prop: number;

  constructor() {
    this.prop = 1; // Works!
  }

  method() {
    this.prop = 1; // Error!
  }
}

const a = new MyClass();
const n: number = a.prop; // Works!
a.prop = 1; // Error!
```

Flow supports [private fields](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Private_class_fields),
a feature of ES2022. Private fields start with a hash symbol `#`:

```js flow-check
class MyClass {
  #internalValue: number;

  constructor() {
    this.#internalValue = 1;
  }

  publicApi() {
    return this.#internalValue;
  }
}

const a = new MyClass();
const x: number = a.#internalValue; // Error!
const y: number = a.publicApi(); // Works!
```

### Extending classes and implementing interfaces

You can optionally `extend` one other class:

```js flow-check
class Base {
  x: number;
}

class MyClass extends Base {
  y: string;
}
```

And also implement multiple [interfaces](../interfaces):

```js flow-check
interface WithXNum {
  x: number;
}
interface Readable {
  read(): string;
}

class MyClass implements WithXNum, Readable {
  x: number;
  read(): string {
    return String(this.x);
  }
}
```

You don't need to `implement` an interface to be a subtype of it, but doing so enforces that your class meets the requirements:

```js flow-check
interface WithXNum {
  x: number;
}

class MyClass implements WithXNum { // Error!
}
```

### Class Constructors {#toc-class-fields-constructors}

You can initialize your class properties in class constructors:

```js flow-check
class MyClass {
  foo: number;

  constructor() {
    this.foo = 1;
  }
}
```

You must first call `super(...)` in a derived class before you can access `this` and `super`:

```js flow-check
class Base {
  bar: number;
}

class MyClass extends Base {
  foo: number;

  constructor() {
    this.foo; // Error
    this.bar; // Error
    super.bar; // Error
    super();
    this.foo; // OK
    this.bar; // OK
    super.bar; // OK
  }
}
```

However, Flow will not enforce that all class properties are initialized in constructors:

```js flow-check
class MyClass {
  foo: number;
  bar: number;

  constructor() {
    this.foo = 1;
  }

  useBar() {
    (this.bar: number); // No errors.
  }
}
```

### Class Generics {#toc-class-generics}

Classes can also have their own [generics](../generics):

```js flow-check
class MyClass<A, B, C> {
  property: A;
  method(val: B): C {
    throw new Error();
  }
}
```

Class generics are [parameterized](../generics#toc-parameterized-generics).
When you use a class as a type you need to pass parameters for each of its
generics:

```js flow-check
class MyClass<A, B, C> {
  constructor(arg1: A, arg2: B, arg3: C) {
    // ...
  }
}

const val: MyClass<number, boolean, string> = new MyClass(1, true, 'three');
```

## Classes in annotations {#toc-classes-in-annotations}

When you use the name of your class in an annotation, it means an _instance_ of your class:

```js
class MyClass {}

const b: MyClass = new MyClass(); // Works!
const a: MyClass = MyClass; // Error!
```

See [here](../utilities#toc-class) for details on `Class<T>`, which allows you
to refer to the type of the class in an annotation.
