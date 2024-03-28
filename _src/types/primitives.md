---
title: Primitive Types
slug: /types/primitives
---

JavaScript has a number of different primitive types
([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Data_structures#primitive_values)):

|   | Example | Flow type |
| - | ------- | --------- |
| **Booleans** | `true` or `false` | `boolean`
| **Strings** | `'foo'` | `string`
| **Numbers** | `123` | `number`
| **Null** | `null` | `null`
| **Undefined** | `undefined` | `void`
| **Symbols** <small>*(new in ES2015)*</small> | `Symbol('foo')` | `symbol`
| **BigInts** <small>*(new in ES2020)*</small> | `123n` | `bigint`

Some primitive types appear in the language as literal values:

```js flow-check
true;
"hello";
3.14;
null;
undefined;
3n;
```

BigInts and Symbols can be created with calls to `BigInt` and `Symbol`, respectively:

```js flow-check
BigInt("2364023476023");
Symbol("hello");
```

The Flow types of literal values are lowercase (mirroring the output of JavaScript's
[`typeof` expression](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof)):

```js flow-check
function func(a: number, b: string, c: boolean, d: bigint) { /* ... */ }

func(3.14, "hello", true, 3n);
```

Some literals can also be used as [literal types](../literals):

```js flow-check
function acceptTwo(x: 2) { /* ... */ }

acceptTwo(2); // Works!
acceptTwo(1); // Error!
```

Some primitives can also be wrapped as objects:

> NOTE: You probably never want to use the wrapper object variants.

```js flow-check
new Boolean(false);
new String("world");
new Number(42);
```

Types for the wrapper objects are capitalized (the same as their constructor):

```js flow-check
function func(x: Number, y: String, z: Boolean) {
  // ...
}

func(new Number(42), new String("world"), new Boolean(false));
```

These wrapper objects are rarely used.

## Booleans {#toc-booleans}

Booleans are `true` and `false` values in JavaScript. The `boolean` type in
Flow accepts these values.

```js flow-check
function acceptsBoolean(value: boolean) { /* ... */ }

acceptsBoolean(true);  // Works!
acceptsBoolean(false); // Works!

acceptsBoolean("foo"); // Error!
```

JavaScript can also implicitly convert other types of values into booleans.

```js
if (42) {} // 42 => true
if ("") {} // "" => false
```

Flow understands these coercions and will allow them as part of an
`if` statement's test or other conditional contexts.

To explicitly convert non-booleans to a `boolean`, you can use `Boolean(x)` or `!!x`.

```js flow-check
function acceptsBoolean(value: boolean) { /* ... */ }

acceptsBoolean(0);          // Error!

acceptsBoolean(Boolean(0)); // Works!
acceptsBoolean(!!0);        // Works!
```

You can [refine](../../lang/refinements/) a value to `boolean` using a `typeof` check:

```js flow-check
function acceptsBoolean(value: boolean) { /* ... */ }

function func(value: mixed) {
  if (typeof value === 'boolean') {
    acceptsBoolean(value); // Works: `value` is `boolean`
  }
}
```

Remember that `boolean` and `Boolean` are different types.

- A `boolean` is a literal value like `true` or `false` or the result of an
  expression like `a === b`.
- A `Boolean` is a wrapper object created by the global `new Boolean(x)`
  constructor. You probably don't want to use this!

## Numbers {#toc-numbers}

Number literals in JavaScript are floating point numbers, for example `42` or `3.14`.
JavaScript also considers `Infinity` and `NaN` to be numbers.
These are represented by the `number` type. JavaScript also has a separate [BigInt type](#toc-bigints).

```js flow-check
function acceptsNumber(value: number) { /* ... */ }

acceptsNumber(42);       // Works!
acceptsNumber(3.14);     // Works!
acceptsNumber(NaN);      // Works!
acceptsNumber(Infinity); // Works!

acceptsNumber("foo");    // Error!
acceptsNumber(123n);     // Error!
```

You can [refine](../../lang/refinements/) a value to `number` using a `typeof` check:

```js flow-check
function acceptsNumber(value: number) { /* ... */ }

function func(value: mixed) {
  if (typeof value === 'number') {
    acceptsNumber(value); // Works: `value` is `number`
  }
}
```

Remember that `number` and `Number` are different types.

- A `number` is a literal value like `42` or `3.14` or the result of an
  expression like `parseFloat(x)`.
- A `Number` is a wrapper object created by the global `new Number(x)` constructor. You probably don't want to use this!

## Strings {#toc-strings}

Strings are `"foo"` values in JavaScript. The `string` type in Flow accepts these values.

```js flow-check
function acceptsString(value: string) { /* ... */ }

acceptsString("foo"); // Works!
acceptsString(`template literal`); // Works!

acceptsString(false); // Error!
```

JavaScript implicitly converts other types of values into strings by
concatenating them.

```js
"foo" + 42; // "foo42"
"foo" + {}; // "foo[object Object]"
```

Flow will only accept strings and numbers when concatenating them to strings.

```js flow-check
"foo" + "foo"; // Works!
"foo" + 42;    // Works!
`foo ${42}`;   // Works!

"foo" + {};    // Error!
"foo" + [];    // Error!
`foo ${[]}`;   // Error!
```

You must be explicit and convert other types into strings. You can do this by
using the String function or using another method for stringifying values.

```js flow-check
"foo" + String({});     // Works!
"foo" + [].toString();  // Works!
"" + JSON.stringify({}) // Works!
```

You can [refine](../../lang/refinements/) a value to `string` using a `typeof` check:

```js flow-check
function acceptsString(value: string) { /* ... */ }

function func(value: mixed) {
  if (typeof value === 'string') {
    acceptsString(value); // Works: `value` is `string`
  }
}
```

Remember that `string` and `String` are different types.

- A `string` is a literal value like `"foo"` or the result of an expression
  like `"" + 42`.
- A `String` is a wrapper object created by the global `new String(x)` constructor. You probably don't want to use this!

## `null` and `undefined` {#toc-null-and-void}

JavaScript has both `null` and `undefined`. Flow treats these as separate
types: `null` and `void` (for `undefined`).

```js flow-check
function acceptsNull(value: null) { /* ... */ }

acceptsNull(null);      // Works!
acceptsNull(undefined); // Error!

function acceptsUndefined(value: void) { /* ... */ }

acceptsUndefined(undefined); // Works!
acceptsUndefined(null);      // Error!
```

You can [refine](../../lang/refinements/) a value to `null` or `void` using equality checks:

```js flow-check
function acceptsNull(value: null) { /* ... */ }

function func(value: mixed) {
  if (value === null) {
    acceptsNull(value); // Works: `value` is `null`
  }
}
```

```js flow-check
function acceptsUndefined(value: void) { /* ... */ }

function func(value: mixed) {
  if (value === undefined) {
    acceptsUndefined(value); // Works: `value` is `void`
  }
}
```

`null` and `void` also appear in other types:

### Maybe types {#toc-maybe-types}

[Maybe types](../maybe) are for places where a value is optional and you can create them by
adding a question mark in front of the type such as `?string` or `?number`.

`?T` is equivalent to `T | null | void`.

```js flow-check
function acceptsMaybeString(value: ?string) { /* ... */ }

acceptsMaybeString("bar");     // Works!
acceptsMaybeString(undefined); // Works!
acceptsMaybeString(null);      // Works!
acceptsMaybeString();          // Works!
```

To refine, `value == null` checks exactly for both `null` and `undefined`.

Read the [maybe type docs](../maybe) for more details.

### Optional object properties {#toc-optional-object-properties}

Object types can have optional properties where a question mark `?` comes after
the property name.

```js
{propertyName?: string}
```

In addition to their set value type, these optional properties can either be
`void` or omitted altogether. However, they cannot be `null`.

```js flow-check
function acceptsObject(value: {foo?: string}) { /* ... */ }

acceptsObject({foo: "bar"});     // Works!
acceptsObject({foo: undefined}); // Works!
acceptsObject({});               // Works!

acceptsObject({foo: null});      // Error!
```

### Optional function parameters {#toc-optional-function-parameters}

Functions can have optional parameters where a question mark `?` comes after
the parameter name.

```js
function func(param?: string) { /* ... */ }
```

In addition to their set type, these optional parameters can either be `void`
or omitted altogether. However, they cannot be `null`.

```js flow-check
function acceptsOptionalString(value?: string) { /* ... */ }

acceptsOptionalString("bar");     // Works!
acceptsOptionalString(undefined); // Works!
acceptsOptionalString();          // Works!

acceptsOptionalString(null);      // Error!
```

### Function parameters with defaults {#toc-function-parameters-with-defaults}

Function parameters can also have defaults. This is a feature of ES2015.

```js
function func(value: string = "default") { /* ... */ }
```

In addition to their set type, default parameters can also be `void` or omitted
altogether. However, they cannot be `null`.

```js flow-check
function acceptsOptionalString(value: string = "foo") { /* ... */ }

acceptsOptionalString("bar");     // Works!
acceptsOptionalString(undefined); // Works!
acceptsOptionalString();          // Works!

acceptsOptionalString(null);      // Error!
```

## Symbols {#toc-symbols}

Symbols are created with `Symbol()` in JavaScript. Flow has basic support for symbols, using the `symbol` type.

```js flow-check
function acceptsSymbol(value: symbol) { /* ... */ }

acceptsSymbol(Symbol()); // Works!
acceptsSymbol(Symbol.isConcatSpreadable); // Works!

acceptsSymbol(false); // Error!
```

You can [refine](../../lang/refinements/) a value to `symbol` using a `typeof` check:

```js flow-check
function acceptsSymbol(value: symbol) { /* ... */ }

function func(value: mixed) {
  if (typeof value === 'symbol') {
    acceptsSymbol(value); // Works: `value` is `symbol`
  }
}
```


## BigInts {#toc-bigints}

BigInts can be used to represent integers of arbitrary precision. In other words, they can store integers which are too large to store as a `number`.

A `bigint` literal is just a `number` literal along with an `n` suffix.

Note that `bigint` and `number` are incompatible types. That is, a `bigint` cannot be used where a `number` is expected, and vice versa.

```js flow-check
function acceptsBigInt(value: bigint) { /* ... */ }

acceptsBigInt(42n); // Works!
acceptsBigInt(42); // Error!
```

You can [refine](../../lang/refinements/) a value to `bigint` using a `typeof` check:

```js flow-check
function acceptsBigInt(value: bigint) { /* ... */ }

function func(value: mixed) {
  if (typeof value === 'bigint') {
    acceptsBigInt(value); // Works: `value` is `bigint`
  }
}
```
