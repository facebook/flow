---
title: Using React with Flow
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

## Component and Hook Syntax {#toc-component-and-hook-syntax}

Flow has dedicated [`component`](./component-syntax.md) and [`hook`](./hook-syntax.md) syntax for
writing React components and hooks, and it is the standard way to write React with Flow. The syntax is
enabled by default, gives components and hooks first-class type system support, and lets Flow enforce
the [Rules of React](https://react.dev/reference/rules) directly.

Make sure your toolchain is set up using the [installation instructions](../getting-started.md#toc-installation),
then read the [Component Syntax](./component-syntax.md) and [Hook Syntax](./hook-syntax.md) docs. The older
[function and class component](./function-and-class-components.md) patterns still type-check, but are legacy:
prefer component and hook syntax for new code.

## Setup Flow with React {#toc-setup-flow-with-react}

Flow and Babel work well together, so it doesn't take much to adopt Flow as a
React user who already uses Babel. If you need to setup Babel with Flow, you can
follow [this guide](../tools/babel.md).

## React Runtimes

Flow supports the `@babel/plugin-transform-react-jsx` runtime options required
to use JSX without explicitly importing the React namespace.

If you are using the new automatic runtime, use this configuration in your `.flowconfig` so
that Flow knows to auto-import `jsx`:

```ini
[options]
react.runtime=automatic
```

## See Also {#toc-see-also}

- [Component Syntax](./component-syntax.md) — Flow's dedicated syntax for declaring React components
- [Hook Syntax](./hook-syntax.md) — Flow's dedicated syntax for declaring custom hooks
- [Component Types](./component-types.md) — typing component references and higher-order patterns
- [Render Types](./render-types.md) — controlling what a component is allowed to render
- [Event Handling](./events.md) — typing event handlers and synthetic events
- [Refs](./refs.md) — typing `useRef`, `createRef`, and callback refs
- [React Types](./types.md) — `React.Node`, `React.MixedElement`, and other built-in types
- [Function and Class Components](./function-and-class-components.md) — legacy patterns for typing components without component syntax
- [Higher-Order Components](./hoc.md) — typing HOC patterns
