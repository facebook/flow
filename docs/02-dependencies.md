---
id: dependencies
title: Using external dependencies
layout: docs
permalink: /docs/dependencies.html
prev: new-project.html
next: library.html
---

Most real JavaScript programs depend on third-party libraries. This guide shows how to use Flow in a project with external dependencies, without having to typecheck library code.

## Interface Files

Flow support *interface files* for this purpose. These files define the interface to a library, including types, separately from the actual code of the library. You never need to change library code to use interface files, but your code will be typechecked against the types declared in the interface file.

The workflow for dealing with library code is:
* Do not change the library files or add `@flow` to them
* Add one or more interface files for your libraries in a special directory in your project - for example `interfaces`
* Point Flow at those interface files by starting it with `flow start --lib  <path to your interface files>`

It is possible to write interface files yourself, but fortunately that is rarely necessary. [DefinitelyTyped](http://definitelytyped.org/) provides TypeScript definition files for many open-source libraries, and Flow can convert those definition files to Flow interface files.

TODO
