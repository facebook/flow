flow-enums-runtime
==================

This is the runtime used with the [Flow Enums](https://flow.org/en/docs/enums/) [Babel transform](https://www.npmjs.com/package/babel-plugin-transform-flow-enums).

Install this package in your regular dependencies, as it is required and used by the output of the Flow Enums transform at runtime.

Read more about how to [enable Flow Enums in your project](https://flow.org/en/docs/enums/enabling-enums/).

### Requirements
This package requires support (either natively or through a polyfill) for [Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map)
and [Array.prototype.values](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values).

Support for [WeakMap](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap) is suggested,
but not required (will fall back to `Map` instead if not present).
