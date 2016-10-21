---
id: existing
title: Running Flow on existing code
permalink: /docs/existing.html
prev: new-project.html
next: type-annotations.html
---

## A word of warning

Making previously-untyped code typecheck with Flow may take some time and work - and sometimes it may not be worth the effort in the short term. Flow supports *interface files* so you can use libraries in a typed way without having to run Flow on them at all. If your project just depends on third party libraries, check out our [guide](third-party.html) on using Flow with external dependencies and consider using an interface file for the libraries.

Why is typechecking existing code so hard? Libraries not written with types in mind often contain complex, highly dynamic code that confuses analyzers such as Flow. The code may also have been written in a style that Flow deliberately chooses not to support in order to give the programmer more help. Some typical examples are:

* Operations on primitive values: While JavaScript allows operations such as `true + 3`, Flow considers it a type error. This is by design, and is done to provide the programmer with more safety. While that's easily avoided for new code, it can sometimes be a lot of effort to eliminate such patterns from existing code.
* Nullability: Flow protects you against accessing properties on `null` by tracking null or undefined values throughout the program. In large existing codebases, though, this can require inserting some extra null checks in places where a value appears like it may be null, but actually isn't at runtime.

It is typically a much larger effort, and requires much more programmer annotation, to get such code to typecheck. On the other hand, if you own a library and would like to benefit from Flow typechecking within the library itself, this guide is for you.

## Weak mode

Flow has a special mode, known as *weak mode*, to get started with complex library code without having to pay the full cost up front. The difference between weak mode and regular mode is how Flow deals with missing type annotations. In regular mode Flow will infer types for all missing annotations, and produce errors whenever it detects a mismatch. In weak mode, Flow will do much less type inference. It will still infer types within functions, but will otherwise treat unannotated variables as having the [`any`](quick-reference.html#any) type - meaning no typechecking happens on them.

A good first step towards typechecking existing library code is to use weak mode, rather than regular mode. Simply change the header declaration in the file you want to typecheck:

```js +line_numbers
/* @flow weak */
```

Weak mode can still point out type problems, and likely will, but there will be far fewer and they will be easier to fix. This should give you some benefit straight away without too much work. The typical errors you are likely to run into are:

* Potentially `null` or `undefined` values, easily fixed by adding conditional checks.
* Primitive type issues, like `true + 3`. Fixing these requires more knowledge of the code but is usually a small, local fix.
* In cases where Flow just doesn't understand the code as written, [explicitly annotating values with `any`](/blog/2015/02/18/Typecasts.html) can help the typechecker.

Weak mode typically produces a manageable number of errors, so you can get down to zero errors in weak mode with moderate effort.

## Next steps

Once your code typechecks with Flow's weak mode, you can start adding type annotations incrementally. Each type annotation you add will allow Flow to validate more code, and you can gradually increase Flow's coverage of your code.

Once your code has more type annotations, you can consider switching it over to regular flow mode by switching the comment at the top of the file to

```js +line_numbers
/* @flow */
```
