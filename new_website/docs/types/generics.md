---
title: Generic Types
slug: /types/generics
---

Generics (sometimes referred to as polymorphic types) are a way of abstracting
a type away.

Imagine writing the following `identity` function which returns whatever value
was passed.

```js
function identity(value) {
  return value;
}
```

We would have a lot of trouble trying to write specific types for this function
since it could be anything.

```js
function identity(value: string): string {
  return value;
}
```

Instead we can create a generic (or polymorphic type) in our function and use
it in place of other types.

```js
function identity<T>(value: T): T {
  return value;
}
```

Generics can be used within functions, function types, classes, type aliases,
and interfaces.

> **Warning:** Flow does not infer generic types. If you want something to have a
generic type, **annotate it**. Otherwise, Flow may infer a type that is less
polymorphic than you expect.

In the following example, we forget to properly annotate `identity` with a generic type, so we run into trouble when we try to assign it to `func`. On the other hand, `genericIdentity` is properly typed, and we are able to use it as expected.

```js flow-check
// @flow

type IdentityWrapper = {
  func<T>(T): T
}

function identity(value) {
  return value;
}

function genericIdentity<T>(value: T): T {
  return value;
}

// $ExpectError
const bad: IdentityWrapper = { func: identity }; // Error!
const good: IdentityWrapper = { func: genericIdentity }; // Works!
```

### Syntax of generics {#toc-syntax-of-generics}

There are a number of different places where generic types appear in syntax.

### Functions with generics {#toc-functions-with-generics}

Functions can create generics by adding the type parameter list `<T>` before
the function parameter list.

You can use generics in the same places you'd add any other type in a function
(parameter or return types).

```js
function method<T>(param: T): T {
  // ...
}

function<T>(param: T): T {
  // ...
}
```

### Function types with generics {#toc-function-types-with-generics}

Function types can create generics in the same way as normal functions, by
adding the type parameter list `<T>` before the function type parameter list.

You can use generics in the same places you'd add any other type in a function
type (parameter or return types).

```js
<T>(param: T) => T
```

Which then gets used as its own type.

```js
function method(func: <T>(param: T) => T) {
  // ...
}
```

### Classes with generics {#toc-classes-with-generics}

Classes can create generics by placing the type parameter list before the body
of the class.

```js flow-check
class Item<T> {
  // ...
}
```

You can use generics in the same places you'd add any other type in a class
(property types and method parameter/return types).

```js flow-check
class Item<T> {
  prop: T;

  constructor(param: T) {
    this.prop = param;
  }

  method(): T {
    return this.prop;
  }
}
```

### Type aliases with generics {#toc-type-aliases-with-generics}

```js flow-check
type Item<T> = {
  foo: T,
  bar: T,
};
```

### Interfaces with generics {#toc-interfaces-with-generics}

```js flow-check
interface Item<T> {
  foo: T,
  bar: T,
}
```

### Supplying Type Arguments to Callables {#toc-supplying-type-arguments-to-callables}

You can give callable entities type arguments for their generics directly in the call:

```js flow-check
//@flow
function doSomething<T>(param: T): T {
  // ...
  return param;
}

doSomething<number>(3);
```

You can also give generic classes type arguments directly in the `new` expression:
```js flow-check
//@flow
class GenericClass<T> {}
const c = new GenericClass<number>();
```

If you only want to specify some of the type arguments, you can use `_` to let flow infer a type for you:

```js flow-check
//@flow
class GenericClass<T, U, V>{}
const c = new GenericClass<_, number, _>()
```

> **Warning:** For performance purposes, we always recommend you annotate with
concrete arguments when you can. `_` is not unsafe, but it is slower than explicitly
specifying the type arguments.

## Behavior of generics {#toc-behavior-of-generics}

### Generics act like variables {#toc-generics-act-like-variables}

Generic types work a lot like variables or function parameters except that they
are used for types. You can use them whenever they are in scope.

```js flow-check
function constant<T>(value: T): () => T {
  return function(): T {
    return value;
  };
}
```

### Create as many generics as you need {#toc-create-as-many-generics-as-you-need}

You can have as many of these generics as you need in the type parameter list,
naming them whatever you want:

```js flow-check
function identity<One, Two, Three>(one: One, two: Two, three: Three) {
  // ...
}
```

### Generics track values around {#toc-generics-track-values-around}

When using a generic type for a value, Flow will track the value and make sure
that you aren't replacing it with something else.

```js flow-check
// @flow
function identity<T>(value: T): T {
  // $ExpectError
  return "foo"; // Error!
}

function identity<T>(value: T): T {
  // $ExpectError
  value = "foo"; // Error!
  // $ExpectError
  return value;  // Error!
}
```

Flow tracks the specific type of the value you pass through a generic, letting
you use it later.

```js flow-check
// @flow
function identity<T>(value: T): T {
  return value;
}

let one: 1 = identity(1);
let two: 2 = identity(2);
// $ExpectError
let three: 3 = identity(42);
```

### Adding types to generics {#toc-adding-types-to-generics}

Similar to  `mixed`, generics have an "unknown" type. You're not allowed to use
a generic as if it were a specific type.

```js flow-check
// @flow
function logFoo<T>(obj: T): T {
  // $ExpectError
  console.log(obj.foo); // Error!
  return obj;
}
```

You could refine the type, but the generic will still allow any type to be
passed in.

```js flow-check
// @flow
function logFoo<T>(obj: T): T {
  if (obj && obj.foo) {
    console.log(obj.foo); // Works.
  }
  return obj;
}

logFoo({ foo: 'foo', bar: 'bar' });  // Works.
logFoo({ bar: 'bar' }); // Works. :(
```

Instead, you could add a type to your generic like you would with a function
parameter.

```js flow-check
// @flow
function logFoo<T: { foo: string }>(obj: T): T {
  console.log(obj.foo); // Works!
  return obj;
}

logFoo({ foo: 'foo', bar: 'bar' });  // Works!
// $ExpectError
logFoo({ bar: 'bar' }); // Error!
```

This way you can keep the behavior of generics while only allowing certain
types to be used.

```js flow-check
// @flow
function identity<T: number>(value: T): T {
  return value;
}

let one: 1 = identity(1);
let two: 2 = identity(2);
// $ExpectError
let three: "three" = identity("three");
```

### Generic types act as bounds {#toc-generic-types-act-as-bounds}

```js flow-check
// @flow
function identity<T>(val: T): T {
  return val;
}

let foo: 'foo' = 'foo';           // Works!
let bar: 'bar' = identity('bar'); // Works!
```

In Flow, most of the time when you pass one type into another you lose the
original type. So that when you pass a specific type into a less specific one
Flow "forgets" it was once something more specific.

```js flow-check
// @flow
function identity(val: string): string {
  return val;
}

let foo: 'foo' = 'foo';           // Works!
// $ExpectError
let bar: 'bar' = identity('bar'); // Error!
```

Generics allow you to hold onto the more specific type while adding a
constraint. In this way types on generics act as "bounds".

```js flow-check
// @flow
function identity<T: string>(val: T): T {
  return val;
}

let foo: 'foo' = 'foo';           // Works!
let bar: 'bar' = identity('bar'); // Works!
```

Note that when you have a value with a bound generic type, you can't use it as
if it were a more specific type.

```js flow-check
// @flow
function identity<T: string>(val: T): T {
  let str: string = val; // Works!
  // $ExpectError
  let bar: 'bar'  = val; // Error!
  return val;
}

identity('bar');
```

### Parameterized generics {#toc-parameterized-generics}

Generics sometimes allow you to pass types in like arguments to a function.
These are known as parameterized generics (or parametric polymorphism).

For example, a type alias with a generic is parameterized. When you go to use
it you will have to provide a type argument.

```js flow-check
type Item<T> = {
  prop: T,
}

let item: Item<string> = {
  prop: "value"
};
```

You can think of this like passing arguments to a function, only the return
value is a type that you can use.

Classes (when being used as a type), type aliases, and interfaces all require
that you pass type arguments. Functions and function types do not have
parameterized generics.

***Classes***

```js flow-check
// @flow
class Item<T> {
  prop: T;
  constructor(param: T) {
    this.prop = param;
  }
}

let item1: Item<number> = new Item(42); // Works!
// $ExpectError
let item2: Item = new Item(42); // Error!
```

***Type Aliases***

```js flow-check
// @flow
type Item<T> = {
  prop: T,
};

let item1: Item<number> = { prop: 42 }; // Works!
// $ExpectError
let item2: Item = { prop: 42 }; // Error!
```

***Interfaces***

```js flow-check
// @flow
interface HasProp<T> {
  prop: T,
}

class Item {
  prop: string;
}

(Item.prototype: HasProp<string>); // Works!
// $ExpectError
(Item.prototype: HasProp); // Error!
```

### Adding defaults to parameterized generics {#toc-adding-defaults-to-parameterized-generics}

You can also provide defaults for parameterized generics just like parameters
of a function.

```js
type Item<T: number = 1> = {
  prop: T,
};

let foo: Item<> = { prop: 1 };
let bar: Item<2> = { prop: 2 };
```

You must always include the brackets `<>` when using the type (just like
parentheses for a function call).

### Variance Sigils {#toc-variance-sigils}

You can also specify the subtyping behavior of a generic via variance sigils.
By default, generics behave invariantly, but you may add a `+` to their
declaration to make them behave covariantly, or a `-` to their declaration to
make them behave contravariantly. See [our docs on variance](../../lang/variance)
for a more information on variance in Flow.

Variance sigils allow you to be more specific about how you intend to
use your generics, giving Flow the power to do more precise type checking.
For example, you may want this relationship to hold:

```js flow-check
//@flow
type GenericBox<+T> = T;

var x: GenericBox<number> = 3;
(x: GenericBox<number| string>);
```

The example above could not be accomplished without the `+` variance sigil:

```js flow-check
//@flow
type GenericBoxError<T> = T;

var x: GenericBoxError<number> = 3;
(x: GenericBoxError<number| string>); // number | string is not compatible with number.
```

Note that if you annotate your generic with variance sigils then Flow will
check to make sure those types only appear in positions that make sense for
that variance sigil. For example, you cannot declare a generic type parameter
to behave covariantly and use it in a contravariant position:

```js flow-check
//@flow
type NotActuallyCovariant<+T> = (T) => void;
```
