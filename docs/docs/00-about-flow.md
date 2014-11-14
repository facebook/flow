---
id: about-flow
title: About Flow
layout: docs
permalink: /docs/about-flow.html
next: getting-started.html
---

Flow is a static type checker for JavaScript. It can be used to catch common bugs in JavaScript programs (e.g., silent type conversions, null dereferences). It also adds syntax to express types of various building blocks (e.g., functions, objects), so that developers can express invariants about their code and have them checked mechanically.

Flow's type checking is opt-in: not all of your files are type checked by default. On the other hand, when you opt-in a file, Flow tries to automatically infer types to the extent possible, without requiring you to do any additional work. This works great if your codebase is broken down into small files, each defining a module that exports a well-defined interface. However, for some files (e.g., monolithic libraries), its analysis might be too imprecise, causing lots of errors to be reported. In such cases, the developer can either try to manually guide Flow with type annotations, or switch to a weaker mode with limited type inference to reduce noise.

Flow's type checking is online: it performs an initial analysis of all files in a code base, and then monitors those files for subsequent changes, type checking them and other other dependencies proactively in the background. For the developer, this means that there are no perceptible compile-time delays; saving a bunch of files in an editor or rebasing a set of files in a repository automatically triggers type checking, storing the results in a persistent server, so that they are available instantaneously when queried.

Flow is very much a work in progress, with many rough edges. Our long-term vision is to make JavaScript development an awesome experience without compromising the essence of the language: we want to give developers powerful tools to understand and maintain their code, and to perform transformations and optimizations easily and safely. We are open-sourcing Flow early in its lifetime so that it has the chance to grow up in public, taking the interests and needs of the entire JavaScript community into account. We hope you will join us in this mission!
