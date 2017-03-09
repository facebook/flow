---
layout: guide
---

JavaScript has a number of different primitive types
([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Data_structures)):

- Booleans
- Strings
- Numbers
- `null`
- `undefined` (`void` in Flow types)
- Symbols (new in ECMAScript 2015, not yet supported in Flow)

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
Boolean(false);
String("world");
Number(42);
Symbol("foo");
```

Types for literal values are lowercase.

```js
// @flow
function method(x: number, y: string, z: boolean) {
  // ...
}

method(3.14, "hello", true);
```

Types for the wrapper objects are capitalized (the same as their constructor).

```js
// @flow
function method(x: Number, y: String, z: Boolean) {
  // ...
}

method(Number(42), String("world"), Boolean(false));
```

These wrapper objects are rarely used.

## Booleans <a class="toc" id="toc-booleans" href="#toc-booleans"></a>

Booleans are `true` and `false` values in JavaScript. The `boolean` type in
Flow accepts these values.

```js
// @flow
function acceptsBoolean(value: boolean) {
  // ...
}

acceptsBoolean(true);  // Works!
acceptsBoolean(false); // Works!
// $ExpectError
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

```js
// @flow
function acceptsBoolean(value: boolean) {
  // ...
}

// $ExpectError
acceptsBoolean(0);          // Error!
acceptsBoolean(Boolean(0)); // Works!
acceptsBoolean(!!0);        // Works!
```

Remember that `boolean` and `Boolean` are different types.

- A `boolean` is a literal value like `true` or `false` or the result of an
  expression like `a === b`.
- A `Boolean` is a wrapper object created by the global `Boolean(x)`
  constructor.

## Numbers <a class="toc" id="toc-numbers" href="#toc-numbers"></a>

Unlike many other languages, JavaScript only has one type of number. These
values may appear as `42` or `3.14`. JavaScript also considers `Infinity` and
`NaN` to be numbers. The `number` type captures everything JavaScript considers
a number.

```js
// @flow
function acceptsNumber(value: number) {
  // ...
}

acceptsNumber(42);       // Works!
acceptsNumber(3.14);     // Works!
acceptsNumber(NaN);      // Works!
acceptsNumber(Infinity); // Works!
// $ExpectError
acceptsNumber("foo");    // Error!
```

Remember that `number` and `Number` are different types.

- A `number` is a literal value like `42` or `3.14` or the result of an
  expression like `parseFloat(x)`.
- A `Number` is a wrapper object created by the global `Number(x)` constructor.

## Strings <a class="toc" id="toc-strings" href="#toc-strings"></a>

Strings are `"foo"` values in JavaScript. The `string` type in Flow accepts
these values.

```js
// @flow
function acceptsString(value: string) {
  // ...
}

acceptsString("foo"); // Works!
// $ExpectError
acceptsString(false); // Error!
```

JavaScript implicitly converts other types of values into strings by
concatenating them.

```js
"foo" + 42; // "foo42"
"foo" + {}; // "foo[object Object]"
```

Flow will only accept strings and number when concatenating them to strings.

```js
// @flow
"foo" + "foo"; // Works!
"foo" + 42;    // Works!
// $ExpectError
"foo" + {};    // Error!
// $ExpectError
"foo" + [];    // Error!
```

You must be explicit and convert other types into strings. You can do this by
using the String method or using another method for stringifying values.

```js
// @flow
"foo" + String({});     // Works!
"foo" + [].toString();  // Works!
"" + JSON.stringify({}) // Works!
```

Remember that `string` and `String` are different types.

- A `string` is a literal value like `"foo"` or the result of an expression
  like `"" + 42`.
- A `String` is a wrapper object created by the global `String(x)` constructor.

## `null` and `void` <a class="toc" id="toc-null-and-void" href="#toc-null-and-void"></a>

JavaScript has both `null` and `undefined`. Flow treats these as separate
types: `null` and `void` (for `undefined`).

```js
// @flow
function acceptsNull(value: null) {
  /* ... */
}

function acceptsUndefined(value: void) {
  /* ... */
}

acceptsNull(null);      // Works!
// $ExpectError
acceptsNull(undefined); // Error!
// $ExpectError
acceptsUndefined(null);      // Error!
acceptsUndefined(undefined); // Works!
```

`null` and `void` also appear in other types.

### Maybe types <a class="toc" id="toc-maybe-types" href="#toc-maybe-types"></a>

Maybe types are for places where a value is optional and you can create them by
adding a question mark in front of the type such as `?string` or `?number`.

In addition to the `type` in `?type`, maybe types can also be `null` or `void`.

```js
// @flow
function acceptsMaybeString(value: ?string) {
  // ...
}

acceptsMaybeString("bar");     // Works!
acceptsMaybeString(undefined); // Works!
acceptsMaybeString(null);      // Works!
acceptsMaybeString();          // Works!
```

### Optional object properties <a class="toc" id="toc-optional-object-properties" href="#toc-optional-object-properties"></a>

Object types can have optional properties where a question mark `?` comes after
the property name.

```js
{ propertyName?: string }
```

In addition to their set value type, these optional properties can either be
`void` or omitted altogether. However, they cannot be `null`.

```js
// @flow
function acceptsObject(value: { optionalProp?: string }) {
  // ...
}

acceptsObject({ foo: "bar" });     // Works!
acceptsObject({ foo: undefined }); // Works!
// $ExpectError
acceptsObject({ foo: null });      // Error!
acceptsObject({});                 // Works!
```

### Optional function parameters <a class="toc" id="toc-optional-function-parameters" href="#toc-optional-function-parameters"></a>

Functions can have optional parameters where a question mark `?` comes after
the parameter name.

```js
function method(param?: string) { /* ... */ }
```

In addition to their set type, these optional parameters can either be `void`
or omitted altogether. However, they cannot be `null`.

```js
// @flow
function acceptsOptionalString(value?: string) {
  // ...
}

acceptsOptionalString("bar");
acceptsOptionalString(undefined);
// $ExpectError
acceptsOptionalString(null);
acceptsOptionalString();
```

### Function parameters with defaults <a class="toc" id="toc-function-parameters-with-defaults" href="#toc-function-parameters-with-defaults"></a>

Function parameters can also have defaults. This is a feature of ECMAScript
2015.

```js
function method(value: string = "default") { /* ... */ }
```

In addition to their set type, default parameters can also be `void` or omitted
altogether. However, they cannot be `null`.

```js
// @flow
function acceptsOptionalString(value: string = "foo") {
  // ...
}

acceptsOptionalString("bar");
acceptsOptionalString(undefined);
// $ExpectError
acceptsOptionalString(null);
acceptsOptionalString();
```

## Symbols <a class="toc" id="toc-symbols" href="#toc-symbols"></a>

Symbols are not currently supported by Flow. You can see these two issues for
more information:

- [facebook/flow#810](https://github.com/facebook/flow/issues/810)
- [facebook/flow#1015](https://github.com/facebook/flow/issues/1015)
