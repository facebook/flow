---
title: Class Types
slug: /types/classes
---

JavaScript [classes](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes)
in Flow operate both as a value and a type.

You write classes the same way you would without Flow, but then you can use the
name of the class as a type.

```js
class MyClass {
  // ...
}

let myInstance: MyClass = new MyClass();
```

This is because classes in Flow are [nominally typed](../lang/nominal-structural).

## Class Syntax {#toc-class-syntax}

Classes in Flow are identical to normal JavaScript classes, but with added
types.

### Class Methods {#toc-class-methods}

Just like in functions, class methods can have annotations for both parameters
(input) and returns (output).

```js
class MyClass {
  method(value: string): number { /* ... */ }
}
```

Also just like regular functions, class methods may have `this` annotations as well.
However, if one is not provided, Flow will infer the class instance type (or the class type for static methods)
instead of `mixed`. When an explicit `this` parameter is provided, it must be a [supertype](../../lang/subtypes/) of
the class instance type (or class type for static methods).

```js
class MyClass {
  method(this : interface { x : string }): void { /* ... */ } // x is missing in `MyClass`
}
```


Unlike class properties, however, class methods cannot be unbound or rebound from
the class on which you defined them. So all of the following are errors in Flow:

```js
let c = new MyClass();
c.method;
let {method} = c;
c.method.bind({});
```

### Class Fields (Properties) {#toc-class-fields-properties}

Whenever you want to use a class field in Flow you must first give it an
annotation.

```js flow-check
// @flow
class MyClass {
  method() {
    // $ExpectError
    this.prop = 42; // Error!
  }
}
```

Fields are annotated within the body of the class with the field name followed
by a colon `:` and the type.

```js flow-check
// @flow
class MyClass {
  prop: number;
  method() {
    this.prop = 42;
  }
}
```

Fields added outside of the class definition need to be annotated within the body
of the class.

```js flow-check
// @flow
function func_we_use_everywhere (x: number): number {
  return x + 1;
}
class MyClass {
  static constant: number;
  static helper: (number) => number;
  function_property: number => number;
}
MyClass.helper = func_we_use_everywhere
MyClass.constant = 42
MyClass.prototype.function_property = func_we_use_everywhere
```

Flow also supports using the [class properties syntax](https://tc39.github.io/proposal-class-public-fields/).

```js flow-check
class MyClass {
  prop = 42;
}
```

When using this syntax, you are not required to give it a type annotation. But
you still can if you need to.

```js flow-check
class MyClass {
  prop: number = 42;
}
```

### Class Generics {#toc-class-generics}

Classes can also have their own [generics](./generics).

```js
class MyClass<A, B, C> {
  property: A;
  method(val: B): C {
    // ...
  }
}
```

Class generics are [parameterized](./generics#toc-parameterized-generics).
When you use a class as a type you need to pass parameters for each of its
generics.

```js
// @flow
class MyClass<A, B, C> {
  constructor(arg1: A, arg2: B, arg3: C) {
    // ...
  }
}

var val: MyClass<number, boolean, string> = new MyClass(1, true, 'three');
```

## Classes in annotations {#toc-classes-in-annotations}

When you use the name of your class in an annotation, it means an _instance_ of your class:

```js
// @flow
class MyClass {}
(MyClass: MyClass); // Error
(new MyClass(): MyClass); // Ok
```

See [here](./utilities#toc-class) for details on `Class<T>`, which allows you
to refer to the type of the class in an annotation.
