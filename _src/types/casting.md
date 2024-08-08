---
title: Type Casting Expressions
slug: /types/casting
---

Sometimes it is useful to assert a type without using something like a function
or a variable to do so. For this Flow supports an inline type cast expression
syntax which can be used in a number of different ways.

## Type Cast Expression Syntax {#toc-type-cast-expression-syntax}

In order to create a type cast expression, use the keyword `as` to cast the value to a type:

```js
value as Type
```

This can also be referred to as an "as expression".

> Before Flow version 0.229, the [legacy syntax](#legacy-casting-syntax) `(value: Type)` was used.

Type cast expressions can appear anywhere an expression can appear:

```js
let val = value as Type;
let obj = {prop: value as Type};
let arr = [value as Type, value as Type] as Array<Type>;
```

The value itself can also be an expression:

```js flow-check
2 + 2 as number;
```

Note that the `as` operator has the same [precedence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table) as `in` and `instanceof`.
Because of this, parentheses around the expression might be required:

```js flow-check
1 === 1 as boolean; // Error!
// Above same as `1 === (1 as boolean)

(1 === 1) as boolean; // Works!
```

Additionally, when in the context of an expression statement, expressions which could ambiguously parse as statements need parens:
```js flow-check
({a: 1}) as {a: number}; // Needs parens to disambiguate from block statement
const x = {a: 1} as {a: number}; // No parens needed, as not in expression statement context
```

When you strip the types all that is left is the value:

```js
value as Type;
```

Is transformed into:
```js
value;
```

### Type Assertions {#toc-type-assertions}

Using type cast expressions you can assert that values are certain types.

```js flow-check
let value = 42;

value as 42;     // Works!
value as number; // Works!
value as string; // Error!
```

Asserting types in this way works the same as types do anywhere else.

### Type Casting {#toc-type-casting}

When you write a type cast expression, the result of that expression is the
value with the provided type. If you hold onto the resulting value, it will
have the new type.

```js flow-check
let value = 42;

value as 42;     // Works!
value as number; // Works!

let newValue = value as number;

newValue as 42;     // Error!
newValue as number; // Works!
```

Unsafe downcasts are not allowed:
```js flow-check
const fooObj = {foo: 1};
const otherObj = fooObj as {foo: number, bar: string};  // ERROR
```

### Adoption of `as` syntax
To use the `as` keyword for type casts, you need to upgrade your infrastructure so that it supports the syntax:
- Flow and Flow Parser: 0.229+
- Prettier: 3.1+
- Babel: use the [babel-plugin-syntax-hermes-parser](https://www.npmjs.com/package/babel-plugin-syntax-hermes-parser) plugin version 0.19+, see our [Babel guide](../../tools/babel) for more details.
- ESLint: use [hermes-eslint](https://www.npmjs.com/package/hermes-eslint) plugin version 0.19+, see our [ESLint guide](../../tools/eslint) for more details.

For more details on how to migrate to the new casting syntax (`as`) check out our [blog post](https://medium.com/flow-type/new-type-casting-syntax-for-flow-as-3ef41567ff3e).

## Using type cast expressions {#toc-using-type-cast-expressions}

> **Note:** We're going to go through a stripped down example for
> demonstrating how to make use of type cast expressions. This example is not
> solved well in practice.

### Type Casting through `any` {#toc-type-casting-through-any}

Because type casts work the same as all other type annotations, you can only
cast values to less specific types. You cannot change the type or make it
something more specific.

But you can use [any](../any) to cast to whatever type you want.

```js flow-check
let value = 42;

value as number; // Works!
value as string; // Error!

let newValue = value as any as string;

newValue as number; // Error!
newValue as string; // Works!
```

By casting the value to `any`, you can then cast to whatever you want.

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

  return clone as any as T;
}

const clone = cloneObject({
  foo: 1,
  bar: true,
  baz: 'three'
});

clone.foo as 1;       // Works!
clone.bar as true;    // Works!
clone.baz as 'three'; // Works!
```

## Legacy casting syntax

Before version 0.229, to create a type cast expression around a `value`, you would
add a colon `:` with the `Type` and wrap the expression with parentheses `(` `)`.

```js
(value: Type)
```

> **Note:** The parentheses are necessary to avoid ambiguity with other syntax.
