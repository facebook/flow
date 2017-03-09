---
id: about-flow
title: About Flow
section: about
layout: default
permalink: /about/
---

## Gradual

Flow's type checking is opt-in, which means you can gradually convert your
existing JavaScript codebase to Flow while reaping incremental benefits. You
don't need to rewrite your existing codebase to start using Flow.

You can opt-in on a per file basis by simply adding a `/* @flow */` comment to
the top of your source file. Flow uses type inference to find errors without
further guidance. You can add type assertions by [annotating your
program](/docs/syntax.html) with types.

## Idiomatic

Flow is designed for JavaScript programmers. Writing code with Flow should feel
like writing JavaScript, from the common idioms in the language to the fast
development cycle.

Most JavaScript code is "boring." Boring is good! Boring code is easy to read
and understand for humans and computers alike. Flow is particularly well suited
to code like this, but that's not all Flow can do.

JavaScript also provides powerful tools for metaprogramming that traditional
statically typed languages lack. Flow is designed to understand even very
dynamic code which is often found in JavaScript programs.

We're constantly improving Flow to understand more and more JavaScript, but if
you need it, the [`any` type](/docs/builtins.html#any) lets you opt-out of type
checking in a granular way, so you can keep writing JavaScript the way
JavaScript was meant to be written.

## Fast

When you start Flow, it performs an initial analysis of all the files in your
codebase and stores the results in a persistent server. When you save a file,
Flow incrementally rechecks the changes in the background.

Both the initial analysis and recheck are heavily optimized for performance,
which preserves the fast feedback of developing plain JavaScript.

In short, you don't need to wait for Flow to check your code.

## Safe

Flow uses control flow analysis to deeply understand your code to find errors
that other type systems can't. Flow is designed to find errors and we take
soundness seriously.

For example, Flow tracks `null` values which may propagate unintentionally
through code and eventually cause a runtime error. Flow's path sensitive
analysis can uncover bugs like this, even through layers of indirection in the
program's control flow.

## Powerful

The type system has a rich and rapidly expanding feature set that allows it to
understand common JavaScript idioms, even when annotations are absent through
type inference.

In addition to inference, Flow also provides an expressive type language,
covering the JavaScript core language and standard libraries found in browsers
and platforms like Node.js.
