---
title: Getting Started
slug: /react
description: Learn how to use Flow to effectively type common and advanced React patterns.
---

Developers will often use Flow and React together, so it is important that Flow
can effectively type both common and advanced React patterns. This guide will
teach you how to use Flow to create safer React applications.

In this guide we will assume you know [the React basics](https://react.dev/learn) and focus on adding
types for patterns you are already familiar with. We will be using examples
based on `react-dom`, but all of these patterns work in other environments
like `react-native` as well.

## Setup Flow with React {#toc-setup-flow-with-react}

Flow and Babel work well together, so it doesn't take much to adopt Flow as a
React user who already uses Babel. If you need to setup Babel with Flow, you can
follow [this guide](../tools/babel/).

## Check Out Component Syntax
Flow supports a dedicated syntax for writing React components and hooks that we recommend instead of using regular
function/class components. Ensure you are set up using our [most up-to-date instructions to
configure your toolchain](../install) and then take a look at the [Component Syntax](./component-syntax)
and [Hook Syntax](./hook-syntax) docs.

## React Runtimes

Flow supports the `@babel/plugin-transform-react-jsx` runtime options required
to use JSX without explicitly importing the React namespace.

If you are using the new automatic runtime, use this configuration in your `.flowconfig` so
that Flow knows to auto-import `jsx`:

```ini
[options]
react.runtime=automatic
```
