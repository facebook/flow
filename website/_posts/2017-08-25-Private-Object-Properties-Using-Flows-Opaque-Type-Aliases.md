---
title: "Private Object Properties Using Flow’s Opaque Type Aliases"
short-title: "Private Props w/ Opaque Types"
author: "Jordan Brown"
medium-link: "https://medium.com/flow-type/private-object-properties-using-flows-opaque-type-aliases-e0100e9b0282"
---
In the last few weeks, a proposal for [private class fields in Javascript](https://github.com/tc39/proposal-class-fields) reached
stage 3. This is going to be a great way to hide implementation details away
from users of your classes. However, locking yourself in to an OOP style of
programming is not always ideal if you prefer a more functional style. Let’s
talk about how you can use Flow’s [opaque type aliases](https://flow.org/en/docs/types/opaque-types/) to get private properties
 on any object type.
