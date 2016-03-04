/* @flow */
/*
---
layout: default
title: Flow | A static type checker for JavaScript
id: home
hero: true
---
*/

/*
## What is Flow?

Flow is a static type checker, designed to quickly find errors in JavaScript
applications:
*/

// $WithLineNums
// @flow
function foo(x) {
  return x * 10;
}
// $ExpectError
foo('Hello, world!');

/*
```bash
$> flow
```
```bbcode
  3:   return x * 10;
              ^ string. This type is incompatible with
  3:   return x * 10;
              ^^^^^^ number
```

Flow also lets you gradually opt-in to statically type checking your code:
*/

// $WithLineNums
// @flow
function bar(x: string, y: number): string {
  // $ExpectError
  return x.length * y;
}
bar('Hello', 42);

/*
```bash
$> flow
```
```bbcode
  3:   return x.length * y;
              ^^^^^^^^^^^^ number. This type is incompatible with
  2: function bar(x: string, y: number): string {
                                         ^^^^^^ string
```

Typed JavaScript code with Flow annotations [easily transforms](/docs/running.html)
down to regular JavaScript, so it runs anywhere.

##Why Flow?

The goal of Flow is to find errors in JavaScript code with little programmer
effort. Flow relies heavily on <strong>type inference</strong> to find type
errors even when the program has not been annotated - it precisely tracks the
types of variables as they flow through the program.

At the same time, Flow is a <strong>gradual</strong> type system. Any parts of
your program that are dynamic in nature can easily bypass the type checker, so
you can mix statically typed code with dynamic code.

Flow also supports a highly <strong>expressive</strong> type language. Flow
types can express much more fine-grained distinctions than traditional type
systems. For example, Flow helps you catch errors involving `null`, unlike most
type systems.

We first introduced Flow at the [@Scale Conference](/docs/running.html) in
September:

<iframe
  frameborder="0"
  allowfullscreen
  width="100%"
  height="320"
  src="http://www.youtube.com/embed/M8x0bc81smU?start=768&showinfo=0&modestbranding=1&rel=0&theme=light">
</iframe>

##Using Flow

Start out with our [Getting Started](/getting-started.html) guide and try Flow
for yourself.

[Flow is open-source](https://github.com/facebook/flow) and still evolving. It
is used heavily within Facebook and we will continue to develop it in the open.
We hope it will be useful for other JavaScript projects, so please try it out,
join the community, and give us feedback!
*/
