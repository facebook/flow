---
layout: guide
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

### Syntax of generics <a class="toc" id="toc-syntax-of-generics" href="#toc-syntax-of-generics"></a>

There are a number of different places where generic types appear in syntax.

##### Functions with generics <a class="toc" id="toc-functions-with-generics" href="#toc-functions-with-generics"></a>

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

##### Function types with generics <a class="toc" id="toc-function-types-with-generics" href="#toc-function-types-with-generics"></a>

Function types can create generics in the same way as normal functions, by
adding the type parameter list `<T>` before the function type parameter list.

You can use generics in the same places you'd add any other type in a function
type (parameter or return types).

```js
<T>(param: T) => T
```

Which then gets used as it's own type.

```js
function method(func: <T>(param: T) => T) {
  // ...
}
```

##### Classes with generics <a class="toc" id="toc-classes-with-generics" href="#toc-classes-with-generics"></a>

Classes can create generics by placing the type parameter list before the body
of the class.

```js
class Item<T> {
  // ...
}
```

You can use generics in the same places you'd add any other type in a class
(property types and method parameter/return types).

```js
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

##### Type aliases with generics <a class="toc" id="toc-type-aliases-with-generics" href="#toc-type-aliases-with-generics"></a>

Classes can create generics by placing the type parameter list before the body
of the class.

```js
type Item<T> = {
  foo: T,
  bar: T,
};
```

##### Interfaces with generics <a class="toc" id="toc-interfaces-with-generics" href="#toc-interfaces-with-generics"></a>

```js
interface Item<T> {
  foo: T,
  bar: T,
}
```

## Behavior of generics <a class="toc" id="toc-behavior-of-generics" href="#toc-behavior-of-generics"></a>

#### Generics act like variables <a class="toc" id="toc-generics-act-like-variables" href="#toc-generics-act-like-variables"></a>

Generic types work a lot like variables or function parameters except that they
are used for types. You can use them whenever they are in scope.

```js
function constant<T>(value: T) {
  return function(): T {
    return value;
  };
}
```

#### Create as many generics as you need <a class="toc" id="toc-create-as-many-generics-as-you-need" href="#toc-create-as-many-generics-as-you-need"></a>

You can have as many of these generics as you need in the type parameter list,
naming them whatever you want:

```js
function identity<One, Two, Three>(one: One, two: Two, three: Three) {
  // ...
}
```

#### Generics track values around <a class="toc" id="toc-generics-track-values-around" href="#toc-generics-track-values-around"></a>

When using a generic type for a value, Flow will track the value and make sure
that you aren't replacing it with something else.

```js
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

```js
// @flow
function identity<T>(value: T): T {
  return value;
}

let one: 1 = identity(1);
let two: 2 = identity(2);
// $ExpectError
let three: 3 = identity(42);
```

#### Adding types to generics <a class="toc" id="toc-adding-types-to-generics" href="#toc-adding-types-to-generics"></a>

Similar to  `mixed` generics have an "unknown" type. You're not allowed to use
a generic as if it were a specific type.

```js
// @flow
function logFoo<T>(obj: T): T {
  // $ExpectError
  console.log(obj.foo); // Error!
  return obj;
}
```

You could refine the type, but the generic will still allow any type to be
passed in.

```js
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

```js
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

```js
// @flow
function identity<T: number>(value: T): T {
  return value;
}

let one: 1 = identity(1);
let two: 2 = identity(2);
// $ExpectError
let three: "three" = identity("three");
```

#### Generic types act as bounds <a class="toc" id="toc-generic-types-act-as-bounds" href="#toc-generic-types-act-as-bounds"></a>

```js
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

```js
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

```js
// @flow
function identity<T: string>(val: T): T {
  return val;
}

let foo: 'foo' = 'foo';           // Works!
let bar: 'bar' = identity('bar'); // Works!
```

Note that when you have a value with a bound generic type, you can't use it as
if it were a more specific type.

```js
// @flow
function identity<T: string>(val: T): T {
  let str: string = val; // Works!
  // $ExpectError
  let bar: 'bar'  = val; // Error!
  return val;
}

identity('bar');
```

#### Parameterized generics <a class="toc" id="toc-parameterized-generics" href="#toc-parameterized-generics"></a>

Generics sometimes allow you to pass types in like arguments to a function.
These are known as parameterized generics (or parametric polymorphism).

For example, a type alias with a generic is parameterized. When you go to use
it you will have to provide a type argument.

```js
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

```js
// @flow
class Item<T> {
  prop: T;
  constructor(param: T) {
    this.prop = param;
  }
}

let item: Item<number> = new Item(42); // Works!
// $ExpectError
let item: Item = new Item(42); // Error!
```

***Type Aliases***

```js
// @flow
type Item<T> = {
  prop: T,
};

let item: Item<number> = { prop: 42 }; // Works!
// $ExpectError
let item: Item = { prop: 42 }; // Error!
```

***Interfaces***

```js
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

##### Adding defaults to parameterized generics <a class="toc" id="toc-adding-defaults-to-parameterized-generics" href="#toc-adding-defaults-to-parameterized-generics"></a>

You can also provide defaults for parameterized generics just like parameters
of a function.

```js
type Item<T: number = 1> = {
  prop: T,
};

let foo: Item<> = { prop:1 };
let bar: Item<2> = { prop: 2 };
```

You must always include the brackets `<>` when using the type (just like
parenthesis for a function call).
