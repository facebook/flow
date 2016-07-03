/* @flow */
/*
---
id: modules
title: Modules
permalink: /docs/modules.html
prev: disjoint-unions.html
next: declarations.html
---
*/

/*
  JavaScript projects in Flow are composed of "modules" which are just single
  files that encapsulate some logic. A module can "**export**"
  variables/functions/classes so that other modules can make use of them.
  Additionally they can "**import**" variables/functions/classes from other
  modules.

  Flow supports both **ES modules (recommended)** and **CommonJS modules**. For
  an excellent explanation of how ES modules (sometimes referred to as
  "ES6 Modules") work, check out
  [the modules chapter](http://exploringjs.com/es6/ch_modules.html) of
  [Exploring JS](http://exploringjs.com/).

  ## ES Modules
  Here's a succinct example of ES modules in action:

*/

// == `Math.js` == //

// This function is exported, so it's available for other modules to import
export function add(num1: number, num2: number): number {
  return num1 + num2;
};

// This function isn't exported, so it's only available in the local scope
// of this module
function sub(num1, num2) {
  return num1 - num2;
}

// Note that we can use both exported and non-exported items within this
// module
var two: number = add(1, 2);
var one: number = sub(2, 1);

/*
  Here we've defined a module by writing a file (`Math.js`). Both functions
  defined within this module are available for use within this file, but only
  `add()` can be imported by *other* modules (because it was exported).

*/

// == `Calculator.js` == //

import {add} from "./Math.js";
// $DocIssue(should eventually be converted to an $ExpectError)
import {sub} from "./Math.js"; // Error! `sub` is not an export of Math.js

var four: number = add(2, 2);

/*
  Because `Math.js` exports its `add()` function, we are able to import it using
  an `import` statement. Similarly, because `Math.js` does *not* export its
  `sub()` function, attempting to import it from another module will result in
  an error. If we wish to import `sub()` into `Calculator.js`, we must export it
  from `Math.js` using the `export` keyword. Note that it is possible to export
  multiple things from one module by just using the `export` keyword on multiple
  things.

  ## CommonJS Modules

  Flow also supports CommonJS modules as well. If you're not familiar with
  CommonJS modules, you can read about them
  [here](https://addyosmani.com/writing-modular-js/) -- but note that we
  recommend using [ES modules](http://exploringjs.com/es6/ch_modules.html)
  if you need to choose between the two options.

  Here's the CommonJS version of the example given above for ES modules:
*/

// == `Math_CommonJS.js` == //

function add(num1: number, num2: number): number {
  return num1 + num2;
};
// This is how we export the `add()` function in CommonJS
exports.add = add;

function sub(num1, num2) {
  return num1 - num2;
}

var two: number = add(1, 2);
var one: number = sub(2, 1);

/*

*/
// == Calculator_CommonJS.js == //

// $DocIssue
var Math = require('./Math_CommonJS.js');

var four: number = Math.add(2, 2);

// Error! `sub` is not exported from Math_CommonJS.js
// $DocIssue $ExpectError: This is an $ExpectError, but the error doesn't show due to a $DocIssue
var one: number = Math.sub(2, 1);

/*
  ### ES Module <-> CommonJS Interoperability

  Some projects start out as CommonJS and wish to migrate to ES modules
  incrementally (or just need to pull in legacy code that uses CommonJS).
  Because of this, Flow supports a set of interoperability semantics between
  the two kinds of module systems. Note that this interop strategy is
  compatible with the strategy employed by Babel 6.

  #### **Importing from CommonJS -> ES Module**

  Say we have the following CommonJS module:
*/

// == CJSModule.js == //

// $DocIssue
class MyClass {}
// $DocIssue
module.exports = MyClass;

/*
  If you wish to import `MyClass` in to an ES module, Flow models this as a
  **default** export from `CJSModule.js`:
*/
// $DocIssue
import MyClass from "./CJSModule.js";
/*
  (Note the lack of curly braces -- showing that this a **"default"** import
   rather than a **"named"** import)

  Now consider a different common pattern that is used in CommonJS modules:
*/

// == CJSModule_MultExports.js == //
function util1() {}
function util2() {}

exports.util1 = util1;
exports.util2 = util2;

/*
  If you wish to import 1 or both of the functions exported by
  `CJSModule_MultExports.js`, you can do so using named import(s):
*/
import {util1, util2} from "./CJSModule_MultExports.js";
/*
  If instead you wish to receive an object with `util1` and `util2` properties
  similar to what you might receive from a call to `require()`, you can do so
  via `import * as`:
*/
// $DocIssue
import * as MultExports from "./CJSModule_MultExports.js";
/*
  #### **Importing from ES Module -> CommonJS**

  Sometimes you're in the middle of converting your project to use ES modules
  and you need to make a change to a legacy module that hasn't been converted
  yet. For this, Flow supports using `require()` to import from an ES module.

  Say we have an ES module with **"named"** exports:
*/
// == ES_NamedExports.js == //
// $DocIssue
export function util1() {}
export function util2() {}
/*
  You can `require()` this ES module from a CommonJS module as follows:
*/
// $DocIssue
const ES_NamedExports = require('./ES_NamedExports.js');

ES_NamedExports.util1();
ES_NamedExports.util2();
/*
  If you have an ES module that has a **"default"** export:
*/
// == ES_DefaultExport.js == //
export default function() {}
/*
  You can `require()` the function as follows:
*/
// $DocIssue
const ES_DefaultExport = require('./ES_DefaultExport.js');

// Note that the default-export is stored as a property named `default`
ES_DefaultExport.default();
/*
  The one rough edge to this CommonJS <-> ES Module interoperability is the fact
  that **"default"** exports from an ES Module will show up as a `default`
  property on the object returned from `require()`.

  This is because, in ES modules, a **"default"** export is essentially just
  sugar for a **"named"** export whose name is `default`.

  ## Module Resolution

  For both ES modules and CommonJS modules, Flow needs to understand how to look
  up the name of a module on disk. For this, Flow uses the same [module
  resolution rules as Node.js](https://nodejs.org/api/modules.html). In other
  words, you most likely don't have to learn or switch to a new module
  resolution system in order to use Flow.

  ### Aliasing Module Names

  In general we recommend against deviating too far from the standard module
  resolution algorithm, but in some advanced environments it is useful to alias
  a module name with some other string before Flow performs the resolution.

  For this we have the **`module.name_mapper`** config option that allows you to
  specify a regular expression template string and a replacement pattern that
  will be run on all `import`s and `require()`s in your project before trying to
  look things up on disk. We call these aliases "name mappers".

  The simplest example of a name mapper is one that will convert all references
  of the `"Foo"` module to references of the `"Bar"` module:

  **`.flowconfig`**

  ```
  [options]
  module.name_mapper='^Foo$' -> 'Bar'
  ```

  **`main.js`**

  ```javascript
  // @flow

  // Because the string "Foo" matches the name mapper above, Flow will look
  // for a module named "Bar" rather than "Foo" here.
  import {something} from "Foo";
  ```

  ### CSS Modules with Webpack

  A more common (and less trivial) example here is to configure Flow to
  understand [CSS Modules](https://github.com/css-modules/css-modules):

  **`CSSModule.js.flow`**

  ```javascript
  // @flow

  // CSS modules have a `className` export which is a string
  declare export var className: string;
  ```

  **`.flowconfig`**

  ```
  [options]
  module.name_mapper='^\(.*\)\.css$' -> '<PROJECT_ROOT>/CSSModule.js.flow'
  ```

  **NOTE: You do not need to manually substitute anything for
          "\<PROJECT\_ROOT\>".** This is a string token that Flow recognizes and
          will automatically replace with the path to the directory of your
          .flowconfig file.

  **`main.js`**

  ```javascript
  // @flow

  import {className} from "./SomeCSSFile.css"; // Works!
  ```

  Note that the **`module.name_mapper`** config option uses the regular
  expression syntax as documented
  [here](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
*/

/*
  ## Type Imports & Exports

  In addition to importing and exporting runtime variables between modules, it
  can also be useful to import and export types as well. For this, Flow has
  extended the ES module `import` and `export` syntax in a couple of ways:

  ### `export type`

  If you wish to define a type alias or interface in a module that other modules
  will need access to as well, you can export it from the module using
  `export type`:

*/

// == User.js == //

export type UserID = number;
export type User = {
  id: UserID,
  name: string,
};

type GuitarT = {
  type: string,
  color: string,
};

export let jimiGuitar: GuitarT = {
  type: "Stratocaster",
  color: "White",
};

export function getUser(id: UserID): User {
  return {
    id: id,
    name: "Jimi Hendrix",
    guitar: jimiGuitar,
  };
}

/*
  Here we've defined `UserID` and `User` as types in `User.js` that are exported
  for other modules to access.

  ### `import type`

  In order for another module to import these types, it must use `import type`:

*/

// $DocIssue
import type {UserID, User} from "./User.js";

/*
  When you use `import type` in a module, you are creating a local type alias to
  the type that you are importing from the other module. `import type` will work
  on type aliases, interfaces, and classes. It will not work on other kinds of
  variables like let/const/var because these do not represent types (they only
  represent values).

  ### `import typeof`

  If you have a value that you'd like to import the type *of*, the most
  straightforward option would be to import the value and then use `typeof` to
  get it's type:
*/

import {jimiguitar} from "./User.js";

// $DocIssue
type GuitarT = typeof jimiguitar;

var myGuitar: GuitarT = {
  type: "Gibson",
  color: "Black",
};

/*
  Alternatively, you can also use the `import typeof` short-hand to make things
  simpler:
*/

// $DocIssue
import typeof {jimiguitar as GuitarT} from "./User.js";

var myGuitar: GuitarT = {
  type: "Gibson",
  color: "Black",
};

/*
  ## Missing/Required Annotations

  Flow is able to infer most types in your program for you, but there is one
  restriction imposed on this rule: You must annotate the exports of a module
  explicitly.

  Flow requires this for 2 reasons:

  1) Placing a type annotation at the boundaries of a module reduces the amount
     of work Flow needs to do to infer the types that span modules across your
     project. This, in turn, removes a performance barrier from Flow's internal
     engine while typechecking your project and makes Flow much faster.

  2) In general, we've found that explicitly annotating module boundaries is
     a good habit to adopt because it helps document the ways in which the
     exports of a module are intended to be used.

  Because of this restriction, you may occasionally see an error from Flow that
  states that you are `Missing an annotation`. To resolve this error, simply add
  type annotations for the export the error points at.
*/
