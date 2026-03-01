---
title: Module Types
slug: /types/modules
---

## Importing and exporting types {#toc-importing-and-exporting-types}

It is often useful to share types between modules (files).
In Flow, you can export type aliases, interfaces, and classes from one file and import them in another.

**`exports.js`**

```js flow-check
export default class MyClass {};
export type MyObject = { /* ... */ };
export interface MyInterface { /* ... */ };
```

**`imports.js`**

```js
import type MyClass, {MyObject, MyInterface} from './exports';
```

> ***Don't forget to add `@flow` at the top of your file, otherwise Flow won't report errors***.

## Importing and exporting values as types {#toc-importing-and-exporting-values}

Flow also supports importing the type of values exported by other modules using [`typeof`](../typeof/).

**`exports.js`**

```js flow-check
const myNumber = 42;
export default myNumber;
export class MyClass { /* ... */ };
```

**`imports.js`**

```js
import typeof MyNumber from './exports';
import typeof {MyClass} from './exports';

const x: MyNumber = 1; // Works: like using `number`
```

Just like other type imports, this code can be stripped away by a compiler so
that it does not add a runtime dependency on the other module.
