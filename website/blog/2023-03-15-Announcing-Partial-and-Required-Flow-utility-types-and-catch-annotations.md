---
title: "Announcing Partial & Required Flow utility types + catch annotations"
short-title: "Announcing Partial & Required Flow utility types"
author: "George Zahariev"
medium-link: "https://medium.com/flow-type/announcing-partial-required-flow-utility-types-catch-annotations-3a32f0bf2a20"
---

:::info[Historical]
This post announced Partial, Required, and catch annotations when it was introduced. For current syntax and behavior, see the [Utility Types](/en/docs/types/utilities) documentation.
:::

Starting in Flow version 0.201, make an object type’s fields all optional using `Partial<ObjType>` (use instead of the unsafe `$Shape`),
and make an object type’s optional fields required with `Required<ObjType>`.
