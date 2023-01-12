---
title: Module Types
slug: /types/modules
---

## Importing and exporting types {#toc-importing-and-exporting-types}

It is often useful to share types in between modules (files). In Flow, you can export type aliases, interfaces, and classes from one file and import them in another.

**`exports.js`**

```js
// @flow
export default class Foo {};
export type MyObject = { /* ... */ };
export interface MyInterface { /* ... */ };
```

**`imports.js`**

```js
// @flow
import type Foo, {MyObject, MyInterface} from './exports';
```

> ***Don't forget to mention `@flow` on top of file, otherwise flow won't report errors***.

## Importing and exporting values {#toc-importing-and-exporting-values}

Flow also supports importing the type of values exported by other modules using
[`typeof`](../typeof/).

**`exports.js`**

```js
// @flow
const myNumber = 42;
export default myNumber;
export class MyClass {
  // ...
}
```

**`imports.js`**

```js
// @flow
import typeof myNumber from './exports';
import typeof {MyClass} from './exports';
```

Just like other type imports, this code will be stripped away by a compiler and
will not add a dependency on the other module.
