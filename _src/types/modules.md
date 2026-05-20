---
title: Module Types
slug: /types/modules
description: "How to import and export Flow types between modules, including type-only imports and CommonJS support."
---

Flow lets you share types between files using `import type` and `export type` syntax.

```js
import type {MyType} from './types';
```

## Importing and exporting types {#toc-importing-and-exporting-types}

You can export [type aliases](./aliases.md), [interfaces](./interfaces.md), and [classes](./classes.md) from one file and import them in another.

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

Flow also supports importing the type of values exported by other modules using [`typeof`](./typeof.md).

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

## See Also {#toc-see-also}

- [Declaration Files](../declarations/index.md) — `.flow` files that declare types for untyped modules
- [Library Definitions](../libdefs/index.md) — declaring types for third-party packages
- [Typeof Types](./typeof.md) — `import typeof` for extracting the type of an imported value
- [Type Aliases](./aliases.md) — defining named types that can be exported and imported
