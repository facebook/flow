---
title: "Flow can now detect unused Promises"
short-title: "Flow can now detect unused Promises"
author: "David Richey"
medium-link: "https://medium.com/flow-type/flow-can-now-detect-unused-promises-b49341256640"
---
As of v0.201.0, Flow can now lint against unused/floating Promises. Unused promises can be dangerous,
because errors are potentially unhandled, and the code may not execute in the intended order. They are
usually mistakes that Flow is perfectly positioned to warn you about.
