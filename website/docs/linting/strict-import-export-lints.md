---
title: Strict ES Module Import/Export Lints
slug: /linting/strict-import-export-lints
description: "Lint rules that enforce stricter ES module import and export usage, requiring the experimental.strict_es6_import_export option."
---

These lint rules enforce stricter usage of ES module `import` and `export` syntax. They all require the [`experimental.strict_es6_import_export`](../../config/options/#toc-experimental-strict-es6-import-export) option to be set to `true` in your `.flowconfig`.

### `default-import-access` {#toc-default-import-access}
Triggers when you access the `default` export of a module indirectly, rather than importing it directly. This includes accessing the `default` property on an `import *` namespace object (via member access or destructuring), and using `import { default as ... }` syntax.

Accessing `default` on a namespace import via member access:
```js
import * as Foo from './foo';

Foo.default; // Error
Foo['default']; // Error
```

Destructuring `default` from a namespace import, including in variable declarations and assignments:
```js
import * as Foo from './foo';

const {default: renamed1} = Foo; // Error
const {'default': renamed2} = Foo; // Error

let x;
({default: x} = Foo); // Error
({'default': x} = Foo); // Error
```

Using `default` in import destructuring:
```js
import {default as renamed} from './foo'; // Error
```

Instead, import the default export directly:
```js
import Foo from './foo'; // Ok
```

### `export-renamed-default` {#toc-export-renamed-default}
Triggers when you set the default export of a module by renaming a named export to `default` in an export specifier, rather than using `export default` directly. This applies to both local exports and re-exports.

Renaming a local binding to `default` in an export specifier:
```js
class Foo {}

export {Foo as default}; // Error
```

Instead, use `export default` directly:
```js
class Foo {}

export default Foo; // Ok
```

Re-exporting a named export as `default`:
```js
export {named1 as default} from './foo'; // Error
```

Re-exporting the `default` property from another module:
```js
export {default} from './foo'; // Error
```

For re-exports, `import` the value first and then use `export default`:
```js
import {named1} from './foo';
export default named1; // Ok
```

### `invalid-import-star-use` {#toc-invalid-import-star-use}
Triggers when an `import *` namespace object is used in a way other than accessing one of its named exports. The namespace object can only be used with member access (e.g. `Foo.bar`), string literal member access (e.g. `Foo['bar']`), or simple destructuring (e.g. `const {bar} = Foo`).

Using the namespace object directly as a value, calling it, or passing it around is not allowed:
```js
import * as Utils from './utils';

const copy = Utils; // Error
Utils(); // Error
```

Using the namespace object in a type position without qualification, or with unqualified `typeof`, is not allowed:
```js
import * as Utils from './utils';

type T = Utils; // Error
type U = typeof Utils; // Error
```

Qualified type access and qualified `typeof` are allowed:
```js
import * as Utils from './utils';

type T = Utils.SomeType; // Ok
type U = typeof Utils.someValue; // Ok
```

Destructuring with rest elements or computed properties is not allowed, because these patterns do not access specific named exports:
```js
import * as Utils from './utils';

const {foo, ...rest} = Utils; // Error
const {foo, [expr]: computed} = Utils; // Error
```

Simple destructuring with known property names is allowed:
```js
import * as Utils from './utils';

const {greeting, count} = Utils; // Ok
const {'greeting': renamed} = Utils; // Ok
```

### `mixed-import-and-require` {#toc-mixed-import-and-require}
Triggers when a file uses both ES module `import` statements and CommonJS `require` calls at the top level. Mixing these two module systems in a single file can lead to confusing semantics and interop issues. Choose one style per file.

Using both `import` and `require` in the same file:
```js
import {named1} from './foo';

const {named2} = require('./foo'); // Error
```

The error points to the `require` call and references the `import` statement. Only the first of each is reported, regardless of how many `import` or `require` statements appear.

Accessing a member on a `require` call also triggers the lint:
```js
import {named1} from './foo';

const named2 = require('./foo').named2; // Error
```

Type-only imports (using `import type` or `import typeof`) do not count as value imports and do not trigger this lint when combined with `require`:
```js
import type {MyType} from './foo';
import typeof {MyClass} from './foo';

const Foo = require('./foo'); // Ok
```

To fix the error, use one module system consistently. Prefer ES module `import` statements:
```js
import {named1} from './foo';
import {named2} from './foo'; // Ok
```

### `non-const-var-export` {#toc-non-const-var-export}
Triggers when you export a variable declared with `var` or `let`. Exported variables should be declared with `const` to prevent reassignment after export, which can lead to unpredictable behavior across modules.

Exporting a `let` or `var` declaration directly:
```js
export let count: number = 0; // Error
export var name: string = 'foo'; // Error

export const MAX: number = 100; // Ok
```

Exporting `let` or `var` variables via export specifiers:
```js
let count: number = 0;
var name: string = 'foo';
const MAX: number = 100;

export {count, name}; // Error on `count` and `name`
export {MAX}; // Ok
```

To fix the error, change the variable declaration to `const`:
```js
export const count: number = 0; // Ok
export const name: string = 'foo'; // Ok
```

### `this-in-exported-function` {#toc-this-in-exported-function}
Triggers when you use `this` in an exported function that does not have a `this` parameter annotation. Without a `this` annotation, the binding of `this` is unknown and can lead to unexpected behavior when the function is called from another module.

Using `this` in a directly exported function declaration:
```js
export function greet() {
  return this.name; // Error
}
```

Using `this` in exported function expressions and arrow functions:
```js
export const greet = function() {
  return this.name; // Error
};

export const greetArrow = () => {
  return this.name; // Error
};
```

Using `this` in a function exported via an export specifier:
```js
function greet() {
  return this.name; // Error
}

export { greet };
```

Using `this` in a default-exported function:
```js
export default function() {
  return this.name; // Error
}
```

To fix the error, add a `this` parameter annotation to the function to explicitly declare the expected `this` type:
```js
export function greet(this: {name: string}) {
  return this.name; // Ok
}
```

Note that `this` in nested function declarations, function expressions, or class bodies within an exported function does not trigger this lint, since those have their own `this` binding:
```js
export function outer() {
  // `this` inside a nested function does not trigger the lint
  function inner() {
    this; // Ok (not flagged by this lint)
  }

  // `this` inside a class body does not trigger the lint
  class C {
    method() {
      this; // Ok
    }
  }
}
```
