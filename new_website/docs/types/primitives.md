---
title: Primitive Types
slug: /types/primitives
---

JavaScript has a number of different primitive types
([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Data_structures)):

- Booleans
- Strings
- Numbers
- `null`
- `undefined` (`void` in Flow types)
- Symbols (new in ECMAScript 2015)

The primitive types appear in the language as either literal values.

```js
true;
"hello";
3.14;
null;
undefined;
```

Or as constructed wrapper objects.

```js
new Boolean(false);
new String("world");
new Number(42);
```

Types for literal values are lowercase.

```js flow-check
// @flow
function method(x: number, y: string, z: boolean) {
  // ...
}

method(3.14, "hello", true);
```

Types for the wrapper objects are capitalized (the same as their constructor).

```js flow-check
// @flow
function method(x: Number, y: String, z: Boolean) {
  // ...
}

method(new Number(42), new String("world"), new Boolean(false));
```

These wrapper objects are rarely used.

## Booleans {#toc-booleans}

Booleans are `true` and `false` values in JavaScript. The `boolean` type in
Flow accepts these values.

```js flow-check
// @flow
function acceptsBoolean(value: boolean) {
  // ...
}

acceptsBoolean(true);  // Works!
acceptsBoolean(false); // Works!
acceptsBoolean("foo"); // Error!
```

JavaScript can also implicitly convert other types of values into booleans.

```js
if (42) {} // 42 => true
if ("") {} // "" => false
```

Flow understands these conversions and will allow any of them as part of an
`if` statement and other types of expressions.

Boolean types need you to be explicit by converting non-booleans. You can do
that with `Boolean(x)` or `!!x`.

```js flow-check
// @flow
function acceptsBoolean(value: boolean) {
  // ...
}

acceptsBoolean(0);          // Error!
acceptsBoolean(Boolean(0)); // Works!
acceptsBoolean(!!0);        // Works!
```

Remember that `boolean` and `Boolean` are different types.

- A `boolean` is a literal value like `true` or `false` or the result of an
  expression like `a === b`.
- A `Boolean` is a wrapper object created by the global `new Boolean(x)`
  constructor.

## Numbers {#toc-numbers}

Unlike many other languages, JavaScript only has one type of number. These
values may appear as `42` or `3.14`. JavaScript also considers `Infinity` and
`NaN` to be numbers. The `number` type captures everything JavaScript considers
a number.

```js flow-check
// @flow
function acceptsNumber(value: number) {
  // ...
}

acceptsNumber(42);       // Works!
acceptsNumber(3.14);     // Works!
acceptsNumber(NaN);      // Works!
acceptsNumber(Infinity); // Works!
acceptsNumber("foo");    // Error!
```

Remember that `number` and `Number` are different types.

- A `number` is a literal value like `42` or `3.14` or the result of an
  expression like `parseFloat(x)`.
- A `Number` is a wrapper object created by the global `new Number(x)` constructor.

## Strings {#toc-strings}

Strings are `"foo"` values in JavaScript. The `string` type in Flow accepts
these values.

```js flow-check
// @flow
function acceptsString(value: string) {
  // ...
}

acceptsString("foo"); // Works!
acceptsString(false); // Error!
```

JavaScript implicitly converts other types of values into strings by
concatenating them.

```js
"foo" + 42; // "foo42"
"foo" + {}; // "foo[object Object]"
```

Flow will only accept strings and number when concatenating them to strings.

```js flow-check
// @flow
"foo" + "foo"; // Works!
"foo" + 42;    // Works!
"foo" + {};    // Error!
"foo" + [];    // Error!
```

You must be explicit and convert other types into strings. You can do this by
using the String method or using another method for stringifying values.

```js flow-check
// @flow
"foo" + String({});     // Works!
"foo" + [].toString();  // Works!
"" + JSON.stringify({}) // Works!
```

Remember that `string` and `String` are different types.

- A `string` is a literal value like `"foo"` or the result of an expression
  like `"" + 42`.
- A `String` is a wrapper object created by the global `new String(x)` constructor.

## `null` and `void` {#toc-null-and-void}

JavaScript has both `null` and `undefined`. Flow treats these as separate
types: `null` and `void` (for `undefined`).

```js flow-check
// @flow
function acceptsNull(value: null) {
  /* ... */
}

function acceptsUndefined(value: void) {
  /* ... */
}

acceptsNull(null);      // Works!
acceptsNull(undefined); // Error!
acceptsUndefined(null);      // Error!
acceptsUndefined(undefined); // Works!
```

`null` and `void` also appear in other types.

### Maybe types {#toc-maybe-types}

Maybe types are for places where a value is optional and you can create them by
adding a question mark in front of the type such as `?string` or `?number`.

In addition to the `type` in `?type`, maybe types can also be `null` or `void`.

```js flow-check
// @flow
function acceptsMaybeString(value: ?string) {
  // ...
}

acceptsMaybeString("bar");     // Works!
acceptsMaybeString(undefined); // Works!
acceptsMaybeString(null);      // Works!
acceptsMaybeString();          // Works!
```

### Optional object properties {#toc-optional-object-properties}

Object types can have optional properties where a question mark `?` comes after
the property name.

```js
{ propertyName?: string }
```

In addition to their set value type, these optional properties can either be
`void` or omitted altogether. However, they cannot be `null`.

```js flow-check
// @flow
function acceptsObject(value: { foo?: string }) {
  // ...
}

acceptsObject({ foo: "bar" });     // Works!
acceptsObject({ foo: undefined }); // Works!
acceptsObject({ foo: null });      // Error!
acceptsObject({});                 // Works!
```

### Optional function parameters {#toc-optional-function-parameters}

Functions can have optional parameters where a question mark `?` comes after
the parameter name.

```js
function method(param?: string) { /* ... */ }
```

In addition to their set type, these optional parameters can either be `void`
or omitted altogether. However, they cannot be `null`.

```js flow-check
// @flow
function acceptsOptionalString(value?: string) {
  // ...
}

acceptsOptionalString("bar");     // Works!
acceptsOptionalString(undefined); // Works!
acceptsOptionalString(null);      // Error!
acceptsOptionalString();          // Works!
```

### Function parameters with defaults {#toc-function-parameters-with-defaults}

Function parameters can also have defaults. This is a feature of ECMAScript
2015.

```js
function method(value: string = "default") { /* ... */ }
```

In addition to their set type, default parameters can also be `void` or omitted
altogether. However, they cannot be `null`.

```js flow-check
// @flow
function acceptsOptionalString(value: string = "foo") {
  // ...
}

acceptsOptionalString("bar");     // Works!
acceptsOptionalString(undefined); // Works!
acceptsOptionalString(null);      // Error!
acceptsOptionalString();          // Works!
```

## Symbols {#toc-symbols}

Symbols are created with `Symbol()` in JavaScript. Flow has basic support for symbols, using the `symbol` type.

```js flow-check
// @flow
function acceptsSymbol(value: symbol) {
  // ...
}

acceptsSymbol(Symbol()); // Works!
acceptsSymbol(Symbol.isConcatSpreadable); // Works!
acceptsSymbol(false); // Error!
```

You can use `typeof x === "symbol"` to refine to a symbol.

```js
const x: symbol | number = Symbol();
if (typeof x === "symbol") {
  const y: symbol = x;
} else {
  const z: number = x;
}
```
