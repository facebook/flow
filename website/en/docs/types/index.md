---
layout: guide
---

Adding type annotations is an important part of your interaction with Flow.

Flow has a powerful ability to infer the types of your programs. The majority
of your code can rely on it. Still, there are places where you'll want to add
types.

Imagine the following `concat` function for concatenating two strings together.

```js
function concat(a, b) {
  return a + b;
}
```

When you use this function, Flow knows exactly what is going on.

```js
concat("A", "B"); // Works!
```

However, you can use the `+` operator on strings or numbers, so this would also
be valid.

```js
concat(1, 2); // Works!
```

But supposed you only want to allow strings in your function. For that you can
add types.

```js
function concat(a: string, b: string) {
  return a + b;
}
```

Now you'll get a warning from Flow if you try to use numbers.

```js
// @flow
function concat(a: string, b: string) {
  return a + b;
}

concat("A", "B"); // Works!
concat(1, 2); // Error!
```

Setting up "boundaries" with your types means you can tell Flow your intent on
top of the inference it already does.

This guide will teach you the syntax and semantics of all the different types
you can have in Flow.
