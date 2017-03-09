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

## Dealing with a large number of type errors

If you encounter too many type errors after running Flow and don't know how to
proceed, you could quickly cut down the number of type errors by adding the
[`any`](quick-reference.html#any) type to parameters and returns of selected
functions in your code. This will have the effect of isolating type errors to
smaller regions of code - for example, within function bodies but not across
function calls - and making them more manageable and easier to fix.

This strategy should give you some benefit straight away without too much
work. The typical errors you are likely to run into are:

* Potentially `null` or `undefined` values, easily fixed by adding conditional checks.
* Primitive type issues, like `true + 3`. Fixing these requires more knowledge of the code but is usually a small, local fix.
* In cases where Flow just doesn't understand the code as written, [explicitly annotating values with `any`](/blog/2015/02/18/Typecasts.html) can help the typechecker.

## Next steps

Once your code typechecks after sufficient fixes and uses of the `any` type, you
can start replacing `any` with better type annotations incrementally. Each type
annotation you add will allow Flow to validate more code, and you can gradually
increase Flow's coverage of your code.
