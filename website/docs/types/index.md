---
title: Type Annotations
slug: /types
description: "Learn how to add Flow type annotations to your code: Primitives, Objects, Functions, Classes, and more."
---

Adding type annotations is an important part of your interaction with Flow.

Flow has a powerful ability to infer the types of your programs. The majority
For example, you don't have to produce annotations for common patterns like `Array.map`:

```js flow-check
["foo", "bar"].map(s => ( // s is inferred to have type string
  s.length
));
```

Still, there are places where you'll want to add types.

Imagine the following `concat` function for concatenating two strings together.

```js flow-check
function concat(a, b) {
  return a + b;
}
```

You need to add annotations on parameters of `concat`, so that Flow can type
check its body. Now you'll get a warning from Flow if you are calling this
function with unexpected types.

```js flow-check
function concat(a: string, b: string) {
  return a + b;
}

concat("A", "B"); // Works!
concat(1, 2); // Error!
```

This guide will teach you the syntax and semantics of all the different types
you can have in Flow.
