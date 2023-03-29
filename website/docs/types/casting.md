---
title: Type Casting Expressions
slug: /types/casting
---

Sometimes it is useful to assert a type without using something like a function
or a variable to do so. For this Flow supports an inline type cast expression
syntax which can be used in a number of different ways.

## Type Cast Expression Syntax {#toc-type-cast-expression-syntax}

In order to create a type cast expression around a `value`, add a colon `:`
with the `Type` and wrap the expression with parentheses `(` `)`.

```js
(value: Type)
```

> **Note:** The parentheses are necessary to avoid ambiguity with other syntax.

Type cast expressions can appear anywhere an expression can appear.

```js
let val = (value: Type);
let obj = { prop: (value: Type) };
let arr = ([(value: Type), (value: Type)]: Array<Type>);
```

The value itself can also be an expression:

```js
(2 + 2: number);
```

When you strip the types all that is left is the value.

```js
(value: Type);
```

```js
value;
```

## Type Assertions {#toc-type-assertions}

Using type cast expressions you can assert that values are certain types.

```js flow-check
let value = 42;

(value: 42);     // Works!
(value: number); // Works!
(value: string); // Error!
```

Asserting types in this way works the same as types do anywhere else.

## Type Casting {#toc-type-casting}

When you write a type cast expression, the result of that expression is the
value with the provided type. If you hold onto the resulting value, it will
have the new type.

```js flow-check
let value = 42;

(value: 42);     // Works!
(value: number); // Works!

let newValue = (value: number);

(newValue: 42);     // Error!
(newValue: number); // Works!
```

## Using type cast expressions {#toc-using-type-cast-expressions}

> **Note:** We're going to go through a stripped down example for
> demonstrating how to make use of type cast expressions. This example is not
> solved well in practice.

### Type Casting through any {#toc-type-casting-through-any}

Because type casts work the same as all other type annotations, you can only
cast values to less specific types. You cannot change the type or make it
something more specific.

But you can use [any](../any) to cast to whatever type you want.

```js flow-check
let value = 42;

(value: number); // Works!
(value: string); // Error!

let newValue = ((value: any): string);

(newValue: number); // Error!
(newValue: string); // Works!
```

By casting the value to any, you can then cast to whatever you want.

This is unsafe and not recommended. But it's sometimes useful when you are
doing something with a value which is very difficult or impossible to type and
want to make sure that the result has the desired type.

For example, the following function for cloning an object.

```js flow-check
function cloneObject(obj: any) {
  const clone: {[string]: mixed} = {};

  Object.keys(obj).forEach(key => {
    clone[key] = obj[key];
  });

  return clone;
}
```

It would be hard to create a type for this because we're creating a new object
based on another object.

If we cast through any, we can return a type which is more useful.

```js flow-check
function cloneObject<T: {+[key: string]: mixed }>(obj: T): T {
  const clone: {[string]: mixed} = {};

  Object.keys(obj).forEach(key => {
    clone[key] = obj[key];
  });

  return ((clone: any): T);
}

const clone = cloneObject({
  foo: 1,
  bar: true,
  baz: 'three'
});

(clone.foo: 1);       // Works!
(clone.bar: true);    // Works!
(clone.baz: 'three'); // Works!
```
