---
layout: guide
---

## What's a "Declaration File"? <a class="toc" id="toc-what-s-a-declaration-file" href="#toc-what-s-a-declaration-file"></a>

Let's look at a more general, and sometimes more convenient, way to
declare types for modules: `.js.flow` files.

The exported types of a module may be declared in a _declaration file_ with
the `.js.flow` extension, colocated with the corresponding _implementation
file_ with the `.js` extension.

A declaration file for a module shadows a
colocated implementation file for that module when typechecking other code
that may depend on that module.


For example, suppose we have the following code in a file `src/LookBeforeYouLeap.js`:

```js
// @flow
import { isLeapYear } from "./Misc";
if (isLeapYear("2020")) console.log("Yay!");
```

Next, suppose that `src/Misc.js` had an incompatible implementation of
`isLeapYear`, just as above.

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

Right, the `isLeapYear` call in `src/LookBeforeYouLeap.js` will typecheck.
As this example shows, declaration files must be written with care: it is up
to the programmer to ensure they are correct, otherwise they may hide type
errors.
That said, declaration files provide a very convenient way to write
specifications for modular typechecking. Sometimes, the implementation code
may not yet be free of type errors, but we may want to move on and come back
to fixing the type errors later. Another important use of this feature is for
libraries, whose implementation code may be too complex to typecheck
satisfactorily, but whose clients we still want to typecheck against
well-defined specifications.

## Inlining declarations in regular code <a class="toc" id="toc-inlining-declarations-in-regular-code" href="#toc-inlining-declarations-in-regular-code"></a>

As noted above, declarations should be distinct from regular code. But
sometimes, it is useful to do declarations "inline," as part of the source of
an implementation file.

**Proceed with caution!**

The most common use is writing "work-in-progress" code while ensuring that
your code typechecks. In the following example, say you want to finish writing
the function `fooList` without bothering to mock up its dependencies first: a
function `foo` that takes a `number`, and returns a `string` and a class
`List` that has a `map` method. Easy! (Just don't forget to replace the
declarations with proper implementations.)

```js
declare class List<T> {
  map<U>(f: (x: T) => U): List<U>;
}
declare function foo(n: number): string;

function fooList(ns: List<number>): List<string> {
  return ns.map(foo);
}
```
