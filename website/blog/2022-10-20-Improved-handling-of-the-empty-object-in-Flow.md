---
title: "Improved handling of the empty object in Flow"
short-title: "Improvements to empty object"
authors: [george-zahariev]
medium-link: "https://medium.com/flow-type/improved-handling-of-the-empty-object-in-flow-ead91887e40c"
---

:::info[Historical]
This post announced improved handling of the empty object when it was introduced. For current syntax and behavior, see the [Objects](/en/docs/types/objects) documentation.
:::

Flow handled the empty object literal {} in a permissive but unsafe way. The fix described in this post increases safety and predictability, but requires using different patterns and behavior.
