---
layout: guide
---

It is often useful to share types in between modules (files). Flow supports
importing and exporting types.

**`exports.js`**

```js
export type MyObject = { /* ... */ };
export interface MyInterface { /* ... */ };
```

**`imports.js`**

```js
import type {MyObject, MyInterface} from './exports';
```

### Importing and exporting types <a class="toc" id="toc-importing-and-exporting-types" href="#toc-importing-and-exporting-types"></a>

In Flow, you can export type aliases and interfaces from one file and import
them in another.

**`exports.js`**

```js
export type MyType = { /* ... */ };
export interface MyInterface { /* ... */ }
```

**`imports.js`**

```js
import type TypeA from './exports';
import type {TypeA} from './exports';
import type {TypeA, TypeB} from './exports';
```

### Importing and exporting values <a class="toc" id="toc-importing-and-exporting-values" href="#toc-importing-and-exporting-values"></a>

Flow also supports importing the type of values exported by other modules using
[`typeof`](../typeof/).

**`exports.js`**

```js
export default const myNumber = 42;
export class MyClass {
  // ...
}
```

**`imports.js`**

```js
import typeof myNumber from './exports';
import typeof {MyClass} from './exports';
```

Just like other type imports, this code will be stripped away by a compiler and
will not add a dependency on the other module.
