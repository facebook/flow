---
title: Typeof Types
slug: /types/typeof
---

JavaScript has a `typeof` operator which returns a string describing a value.

```js
typeof 1 === 'number'
typeof true === 'boolean'
typeof 'three' === 'string'
```

However it is limited in that this string only describes so much about the
type.

```js
typeof { foo: true } === 'object'
typeof { bar: true } === 'object'
typeof [true, false] === 'object'
```

In Flow, there is a similar `typeof`  operator, but it's much more powerful.

## `typeof` type syntax {#toc-typeof-type-syntax}


The `typeof` operator returns the Flow type of a given value to be used as a
type.

```js flow-check
// @flow
let num1 = 42;
let num2: typeof num1 = 3.14;     // Works!
// $ExpectError
let num3: typeof num1 = 'world';  // Error!

let bool1 = true;
let bool2: typeof bool1 = false;  // Works!
// $ExpectError
let bool3: typeof bool1 = 42;     // Error!

let str1 = 'hello';
let str2: typeof str1 = 'world'; // Works!
// $ExpectError
let str3: typeof str1 = false;   // Error!
```

You can use any value with `typeof`:

```js flow-check
// @flow
let obj1 = { foo: 1, bar: true, baz: 'three' };
let obj2: typeof obj1 = { foo: 42, bar: false, baz: 'hello' };

let arr1 = [1, 2, 3];
let arr2: typeof arr1 = [3, 2, 1];
```

## `typeof` inherits behaviors of inference {#toc-typeof-inherits-behaviors-of-inference}

Flow does all sorts of type inference on your code so that you don't have to
type annotate anything. Generally, inference avoids getting in your way while
still preventing you from introducing bugs.

But when you use `typeof`, you're taking the results of Flow's inference and
asserting it as a type. While this can be very useful, it can also lead to some
unexpected results.

For example, when you use literal values in Flow, their inferred type is the
primitive that it belongs to. Thus, the number 42 has the inferred type of
`number`. You can see this when you use `typeof`.

```js flow-check
// @flow
let num1 = 42;
let num2: typeof num1 = 3.14;    // Works!

let bool1 = true;
let bool2: typeof bool1 = false; // Works!

let str1 = 'hello';
let str2: typeof str1 = 'world'; // Works!
```

However, this only happens with the inferred type. If you specify the literal
type, it will be used in `typeof`.

```js flow-check
// @flow
let num1: 42 = 42;
// $ExpectError
let num2: typeof num1 = 3.14;    // Error!

let bool1: true = true;
// $ExpectError
let bool2: typeof bool1 = false; // Error!

let str1: 'hello' = 'hello';
// $ExpectError
let str2: typeof str1 = 'world'; // Error!
```

## `typeof` inherits behaviors of other types {#toc-typeof-inherits-behaviors-of-other-types}

There are many different types in Flow, some of these types behave differently
than others. These differences make sense for that particular type but not for
others.

When you use `typeof`, you're inserting another type with all of its behaviors.
This can make `typeof` seem inconsistent where it is not.

For example, if you use `typeof` with a class you need to remember that classes
are *nominally* typed instead of *structurally* typed. So that two classes with
the same exact shape are not considered equivalent.

```js flow-check
// @flow
class MyClass {
  method(val: number) { /* ... */ }
}

class YourClass {
  method(val: number) { /* ... */ }
}

// $ExpectError
let test1: typeof MyClass = YourClass; // Error!
let test2: typeof MyClass = MyClass;   // Works!
```
