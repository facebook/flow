---
layout: guide
---

## What's a Declaration File? <a class="toc" id="toc-what-s-a-declaration-file" href="#toc-what-s-a-declaration-file"></a>

Let's look at a more general, and sometimes more convenient way to
declare types for modules: `.flow` files.

There are two possible use cases, depending on whether an implementation file exists
or not.

In the first case, the exported types of a module are declared in a _declaration
file_ `<FILENAME>.flow`, that is located in the same directory as the corresponding _implementation
file_ `<FILENAME>`. The declaration file completely shadows the colocated
implementation. In other words, Flow will completely ignore `<FILENAME>` and just
read `<FILENAME>.flow` instead.

In the second case, the implementation file is missing entirely. `<FILENAME>.flow`
is treated as if it is named `<FILENAME>`.

Note that the `.flow` extension applies both to `.js` files as well as `.json`
ones. The corresponding declaration files have extensions `.js.flow` and `.json.flow`,
respectively.

Now let's see an example of the first case documented above. Suppose we have the
following code in a file `src/LookBeforeYouLeap.js`:

```js
// @flow
import { isLeapYear } from "./Misc";
if (isLeapYear("2020")) console.log("Yay!");
```

and suppose that `src/Misc.js` has an incompatible implementation of `isLeapYear`:

```js
// @flow
export function isLeapYear(year: number): boolean {
  return year % 4 == 0; // yeah, this is approximate
}
```

If we now create a declaration file `src/Misc.js.flow`, the declarations in it
will be used instead of the code in `src/Misc.js`. Let's say we have the
following declarations in `src/Misc.js.flow`.

> NOTE: The syntax for declarations in a declaration file is the same as we've seen in
>       [Creating Library Definitions section](../libdefs/creation).

```js
// @flow
declare export function isLeapYear(year: string): boolean;
```

What do you think will happen?

Right, the `isLeapYear` call in `src/LookBeforeYouLeap.js` will typecheck, since
the `year` parameter expects a `string` in the declaration file.

As this example shows, declaration files must be written with care: it is up
to the programmer to ensure they are correct, otherwise they may hide type
errors.


## Inlining declarations in regular code <a class="toc" id="toc-inlining-declarations-in-regular-code" href="#toc-inlining-declarations-in-regular-code"></a>

Sometimes it is useful to make declarations inline, as part of the source of
an implementation file.

In the following example, say you want to finish writing
the function `fooList` without bothering to mock up its dependencies first: a
function `foo` that takes a `number` and returns a `string`, and a class
`List` that has a `map` method. You can do this by including declarations for
`List` and `foo`:

```js
declare class List<T> {
  map<U>(f: (x: T) => U): List<U>;
}
declare function foo(n: number): string;

function fooList(ns: List<number>): List<string> {
  return ns.map(foo);
}
```

Just don't forget to replace the declarations with proper implementations.
