---
id: about-flow
title: About Flow
layout: docs
permalink: /docs/about-flow.html
next: installing-flow.html
---

Flow is a static type checker for JavaScript. It can be used to catch common bugs in JavaScript programs (e.g., silent type conversions, null dereferences), often without requiring any changes to your code. It also adds type syntax to JavaScript, so that developers can express invariants about their code and have them maintained automatically.

Flow's type checking is opt-in: files are not type checked unless you ask it to. This means that you can gradually convert your JavaScript codebase to Flow while reaping incremental benefits. When you do opt-in a file, Flow tries to type check the code automatically by performing type inference, reporting errors without further manual guidance. This simple workflow is usually sufficient when your codebase is broken down into small files, each defining a module that exports a well-defined interface. However, for some files (e.g., monolithic libraries), the analysis Flow performs might turn out to be too imprecise, causing lots of spurious errors to be reported. In such cases, the developer can either try to manually guide Flow with type annotations, or switch to a weaker mode with limited type inference to reduce noise.

Flow's type checking is online: it performs an initial analysis of all files in a code base, and then monitors those files for subsequent changes, type checking them and other other dependencies proactively in the background. For the developer, this means that there are no perceptible compile-time delays; saving a bunch of files in an editor or rebasing a set of files in a repository automatically triggers type checking, storing the results in a persistent server, so that they are available instantaneously when queried.

Flow is very much a work in progress, with many rough edges. Our long-term vision is to make JavaScript development an awesome experience without compromising the essence of the language: we want to give developers powerful tools to understand and maintain their code, and to perform transformations and optimizations easily and safely. We are open-sourcing Flow early in its lifetime so that it has the chance to grow up in public, taking the interests and needs of the entire JavaScript community into account. We hope you will join us in this mission!
