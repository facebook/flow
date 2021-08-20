---
title: Getting Started
slug: /
---

Flow is a static type checker for your JavaScript code. It does a lot of work
to make you more productive. Making you code faster, smarter, more confidently,
and to a bigger scale.

Flow checks your code for errors through **static type annotations**. These
_types_ allow you to tell Flow how you want your code to work, and Flow will
make sure it does work that way.

```js flow-check
// @flow
function square(n: number): number {
  return n * n;
}

square("2"); // Error!
```

Because Flow understands JavaScript so well, it doesn't need many of these
types. You should only ever have to do a minimal amount of work to describe your
code to Flow and it will _infer_ the rest. A lot of the time, Flow can
understand your code without any types at all.

```js flow-check
// @flow
function square(n) {
  return n * n; // Error!
}

square("2");
```

You can also adopt Flow incrementally and easily remove it at
anytime, so you can try Flow out on any codebase and see how you like it.
