---
title: Variable Declarations
slug: /lang/variables
---

import {SinceVersion} from '../../components/VersionTags';

When you are declaring a new variable, you may optionally declare its type.

JavaScript has three ways of declaring local variables:

- `var` - declares a variable, optionally assigning a value.
  ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var))
- `let` - declares a block-scoped variable, optionally assigning a value.
  ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let))
- `const` - declares a block-scoped variable, assigning a value that cannot be re-assigned.
  ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/const))

In Flow these fall into two groups:

- `let` and `var` - variables that **can** be reassigned.
- `const` - variables that **cannot** be reassigned.

```js flow-check
var varVariable = 1;
let letVariable = 1;
const constVariable = 1;

varVariable = 2;   // Works!
letVariable = 2;   // Works!
constVariable = 2; // Error!
```

## `const` {#toc-const}

Since a `const` variable cannot be re-assigned at a later time it is fairly
simple.

Flow can either infer the type from the value you are assigning to it or you
can provide it with a type.

```js flow-check
const foo /* : number */ = 1;
const bar: number = 2;
```

## `var` and `let` <SinceVersion version="0.186" /> {#toc-var-and-let}
Since `var` and `let` can be re-assigned, there's a few more rules you'll need
to know about.

When you provide a type, you will be able to re-assign the value, but it must
always be of a compatible type.

```js flow-check
let foo: number = 1;
foo = 2;   // Works!
foo = "3"; // Error!
```

When the variable has no annotation, Flow infers a precise type based on
 their initializer or initial assignment. All subsequent assignments
to that variable will be constrained by this type. This section shows some examples
of how Flow determines what type an unannotated variable is inferred to have.


**If you want a variable to have a different type than what Flow infers for it,
you can always add a type annotation to the variable’s declaration. That will
override everything discussed in this page!**

### Variables initialized at their declarations {#toc-variables-initialized-at-their-declarations}

The common case  for unannotated variables is very straightforward: when a
variable is declared with an initializer that is not the literal `null`, that
variable will from then on have the type of the initializer, and future writes
to the variable will be constrained by that type.

```js flow-check
import * as React from 'react';

type Props = $ReadOnly<{ prop: string }>;

declare var x: number;
declare var y: number;
declare var props: Props;

let product = Math.sqrt(x) + y;
// `product` has type `number`

let Component = ({prop}: Props): React.Node => { return <div/> }
// `Component` has type`React.ComponentType<Props>`

let element = <Component {...props} />
// `element` has type `React.Element<React.ComponentType<Props>>`

/* Let's define a new component */

type OtherProps = $ReadOnly<{ ...Props, extra_prop: number }>;
declare var OtherComponent: (OtherProps) => React.Node;
declare var other_props: OtherProps

/* Any subsequent assignments to `product`, `Component`, or `element` will be
 * checked against the types that Flow infers for the initializers, and if
 * conflicting types are assigned, Flow will signal an error. */

product = "Our new product is...";
Component = ({prop}: OtherProps): React.Node => { return <div/> };
element = <OtherComponent {...other_props} />;
```

If you want these examples to typecheck, and for Flow to realize that different
kinds of values can be written to these variables, you must add a type
annotation reflecting this more general type to their declarations:
```js
let product: number | string = ...
let Component: mixed = ... // No good type to represent this! Consider restructuring
let element: React.Node = ...
```
### Variables declared without initializers {#toc-variables-declared-without-initializers}

Often variables are declared without initializers. In such cases, Flow will try
to choose the "first" assignment or assignments to the variable to define its
type. "First" here means both top-to-bottom and nearer-scope to
deeper-scope—we’ll try to choose an assignment that happens in the same function
scope as the variable’s declaration, and only look inside nested functions if we
don’t find any assignments locally:
```js flow-check
let topLevelAssigned;
function helper() {
  topLevelAssigned = 42; // Error: `topLevelAssigned` has type `string`
}
topLevelAssigned = "Hello world"; // This write determines the var's type
topLevelAssigned = true; // Error: `topLevelAssigned` has type `string`
```
If there are two or more possible "first assignments," due to an `if`- or
`switch`-statement, they’ll both count—this is one of the few ways that Flow
will still infer unions for variable types:
```js flow-check
let myNumberOrString;
declare var condition: boolean;
if (condition) {
  myNumberOrString = 42; // Determines type
} else {
  myNumberOrString = "Hello world"; // Determines type
}
myNumberOrString = 21; // fine, compatible with type
myNumberOrString = "Goodbye"; // fine, compatible with type
myNumberOrString = false; // Error: `myNumberOrString` has type `number | string`
```
This only applies when the variable is written to in both branches, however. If
only one branch contains a write, that write becomes the type of the variable
afterwards (though Flow will still check to make sure that the variable is
definitely initialized):

```js flow-check
let oneBranchAssigned;
declare var condition: boolean;
if (condition) {
  oneBranchAssigned = "Hello world!";
}
oneBranchAssigned.toUpperCase(); // Error: `oneBranchAssigned` may be uninitialized
oneBranchAssigned = 42; // Error: `oneBranchAssigned` has type `string`
```
### Variables initialized to `null` {#toc-variables-initialized-to-null}

Finally, the one exception to the general principle that variable’s types are
determined by their first assignment(s) is when a variable is initialized as (or
whose first assignment is) the literal value `null`. In such cases, the *next*
non-null assignment (using the same rules as above) determines the rest of the
variable’s type, and the overall type of the variable becomes a union of `null`
and the type of the subsequent assignment. This supports the common pattern
where a variable starts off as `null` before getting assigned by a value of some
other type:
```js flow-check
function findIDValue<T>(dict: {[key: string]: T}): T {
  let idVal = null; // initialized as `null`
  for (const key in dict) {
    if (key === 'ID') {
      idVal = dict[key]; // Infer that `idVal` has type `null | T`
    }
  }
  if (idVal === null) {
    throw new Error("No entry for ID!");
  }
  return idVal;
}
```

## Catch variables <SinceVersion version="0.197" />
If a `catch` variable does not have an annotation, its default type is [`any`](../../types/any).

You can optionally annotate it with exactly [`mixed`](../../types/mixed) or `any`. E.g.

```js flow-check
try {
} catch (e: mixed) {
  if (e instanceof TypeError) {
    e as TypeError; // OK
  } else if (e instanceof Error) {
    e as Error; // OK
  } else {
    throw e;
  }
}
```

By using `mixed`, you can improve your safety and Flow [coverage](../../cli/coverage/),
at the trade-off of increased runtime checks.

You can change the default type of `catch` variables when there is no annotation by setting the [`use_mixed_in_catch_variables`](../../config/options/#toc-use-mixed-in-catch-variables) option to true.
