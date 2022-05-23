---
title: Announcing Typecasts
short-title: Typecasts
author: Basil Hosmer
hide_table_of_contents: true
---

As of version 0.3.0, Flow supports typecast expression.

A typecast expression is a simple way to type-annotate any JavaScript expression. Here are some examples of typecasts:

```JavaScript
(1 + 1 : number);
var a = { name: (null: ?string) };
([1, 'a', true]: Array<mixed>).map(fn);
```

For any JavaScript expression `<expr>` and any Flow type `<type>`, you can write

```JavaScript
(<expr> : <type>)
```

**Note:** the parentheses are necessary.

<!--truncate-->

## How Typecasts Work

To evaluate a typecast expression, Flow will first check that `<expr>` is a `<type>`.

```JavaScript
(1+1: number); // this is fine
(1+1: string); // but this is is an error
```

Second, Flow will infer that the typecast expression `(<expr>: <type>)` has the type `<type>`.

```JavaScript
[(0: ?number)]; // Flow will infer the type Array<?number>
[0];            // Without the typecast, Flow infers the type Array<number>
```

## Safety

Typecasts obey the same rules as other type annotations, so they provide the same safety guarantees. This means they are safe unless you explicitly use the `any` type to defeat Flow's typechecking. Here are examples of upcasting (which is allowed), downcasting (which is forbidden), and using `any`.

```JavaScript
class Base {}
class Child extends Base {}
var child: Child = new Child();

// Upcast from Child to Base, a more general type: OK
var base: Base = new Child();

// Upcast from Child to Base, a more general type: OK
(child: Base);

// Downcast from Base to Child: unsafe, ERROR
(base: Child);

// Upcast base to any then downcast any to Child.
// Unsafe downcasting from any is allowed: OK
((base: any): Child);
```

## More examples

Typecasts are particularly useful to check assumptions and help Flow infer the types you intend. Here are some examples:

```JavaScript
(x: number) // Make Flow check that x is a number
(0: ?number) // Tells Flow that this expression is actually nullable.
(null: ?number) // Tells Flow that this expression is a nullable number.
```

## Transformations

Like type annotations and other Flow features, typecasts need to be transformed away before the code can be run. The transforms will be available in react-tools `0.13.0` when it is published soon, but for now they're available in `0.13.0-beta.2`, which you can install with

```bash
npm install react-tools@0.13.0-beta.2
```
