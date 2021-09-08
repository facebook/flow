---
title: Library Definitions
slug: /libdefs
---

## What's a "Library Definition"? {#toc-what-s-a-library-definition}

Most real JavaScript programs depend on third-party code and not just code
immediately under the control of the project. That means a project using Flow
may need to reference outside code that either doesn't have type information or
doesn't have accurate and/or precise type information. In order to handle this,
Flow supports the concept of a "library definition" (AKA "libdef").

A libdef is a special file that informs Flow about the type signature of some
specific third-party module or package of modules that your application uses.
If you're familiar with languages that have header files (like `C++`), you can
think of libdefs as a similar concept.

These special files use the same `.js` extension as normal JS code, but they are
placed in a directory called `flow-typed` in the root directory of your project.
Placement in this directory tells Flow to interpret them as libdefs rather than
normal JS files.

> NOTE: Using the `/flow-typed` directory for libdefs is a convention that
>       enables Flow to JustWork™ out of the box and encourages consistency
>       across projects that use Flow, but it is also possible to explicitly
>       configure Flow to look elsewhere for libdefs using the [`[libs]` section
>       of your `.flowconfig`](../config/libs).

## General Best Practices {#toc-general-best-practices}

**Try to provide a libdef for each third-party library your project uses**

If a third-party library that has no type information is used by your project,
Flow will treat it like any other untyped dependency and mark all of its exports
as `any`. Interestingly, this is the only place that Flow will implicitly inject
`any` into your program.

Because of this behavior, it is a best practice to find or write libdefs for as
many of the third-party libraries that you use as you can. We recommend checking
out the `flow-typed`
[tool and repository](https://github.com/flowtype/flow-typed/blob/master/README.md)
, which helps you quickly find and install pre-existing libdefs for your
third-party dependencies.
