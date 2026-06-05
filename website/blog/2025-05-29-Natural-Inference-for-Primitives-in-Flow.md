---
title: "Natural Inference for Primitives in Flow"
short-title: "Natural Inference for Primitives"
authors: [panagiotis-vekris]
medium-link: "https://medium.com/flow-type/flow-natural-inference-for-primitives-df27149109bb"
---

:::info[Historical]
This post announced natural inference for primitives when it was introduced. For current behavior, see the [literal types](/en/docs/types/literals) and [`const` expressions and `const` type parameters](/en/docs/types/const-expression) documentation.
:::

Flow now decides the type of string, number, and boolean literal values at their definition site: either the broad type (e.g. `string`) or the literal type (e.g. `'foo'`). You can control the behavior with type annotations, `as const`, or `const` type parameters.

<!--truncate-->
