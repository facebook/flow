---
title: "Improved handling of the empty object in Flow"
short-title: "Improvements to empty object"
author: "George Zahariev"
medium-link: "https://medium.com/flow-type/improved-handling-of-the-empty-object-in-flow-ead91887e40c"
---
Flow handled the empty object literal {} in a permissive but unsafe way. The fix described in this post increases safety and predictability, but requires using different patterns and behavior.
