---
id: library
title: Running Flow on a library
layout: docs
permalink: /docs/library.html
prev: new-project.html
next: transpiler.html
---

## A word of warning

Making existing libraries typecheck with Flow is not for the faint of heart - and most of the time you don't need to do it. If your project just depends on a library, check out our [guide](dependencies.html) on using Flow with external dependencies. Flow supports *interface files* so you can use libraries in a typed way without having to run Flow on them at all.

Why is typechecking existing libraries so hard? Libraries not written with types in mind often contain complex, highly dynamic code that confuse analyses such as Flow. The code may also have been written in a style that Flow deliberately chooses not to support in order to give the programmer more help. Some typical examples are:

* Operations on primitive values: while Javascript allows operations such as `true + 3`, Flow considers it a type error. This is by design, and is done to provide the programmer with more safety. While that's easily avoided for new code, it can be a lot of effort to eliminate such patterns from existign code.
* Nullability: Flow protects you against accessing properties on `null` by tracking null or undefined values throughout the program. In large existing codebases though this can require inserting some extra null checks in places where a value appears null but isn't at runtime.

It is typically a much larger effort, and requires much more programmer annotation, to get such code to typecheck. On the other hand, if you own a library and would like to benefit from Flow typechecking within the library itself, this guide is for you.

## Weak mode

Flow has a special mode, known as *weak mode*, to get started with complex library code without having to pay the full cost up front. The difference between weak mode and regular mode is how Flow deals with missing type annotations. In regular mode Flow will infer types for all missing annotations, and produce errors whenever it detects a mismatch. In weak mode, Flow will do much less type inference. It will still infer types within functions, but will otherwise treat unannotated variables as having the `any` type - meaning no typechecking happens on them.

The first step towards typechecking existing library code is to use weak mode, rather than regular mode. Simply change the header declaration in the file you want to typecheck:

{% highlight javascript linenos %}
/* @flow weak */
{% endhighlight %}

Weak mode can still point out type problems, and likely will, but these will be far fewer and easier to fix. You therefore get some benefit straight away without too much work. The typical errors you are likely to run into are:

* Potentially `null` or `undefined` values, easily fixed by adding checks.
* Primitive type issues, like `true + 3`. Fixing these requires more knowledge of the code but is usually a small, local fix.
* In cases where Flow just doesn't understand the code as written, explicitly annotating values with `any` can help the typechecker.

Weak mode typically produces a manageable number of errors, so you can get down to zero errors in weak mode with moderate effort.

## Next steps

Once your code typechecks with Flow's weak mode, you can start adding type annotations incrementally. Each type annotation you add will add more typechecking, and you can gradually increase Flow's coverage of your code.

Once your code has more type annotations, you can consider switching it over to regular flow mode by switching the comment at the top of the file to

{% highlight javascript linenos %}
/* @flow */
{% endhighlight %}

Doing this too early will likely yield an unmanageable number of errors, however.
