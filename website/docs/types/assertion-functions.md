---
title: Assertion Functions
slug: /types/assertion-functions
description: "How to write functions that assert a type or condition and narrow values after a successful call."
---

import {SinceVersion} from '../../components/VersionTags';

Assertion functions narrow a value's type after a call returns normally. Use them to check a value and throw an error when the check fails.

Unlike [type guards](./type-guards.md), which return a boolean for use in a conditional, assertion functions return `void`. The `asserts` annotation tells Flow how to narrow values after a successful call.

## Basic Usage <SinceVersion version="0.336" /> {#toc-basic-usage}

Annotate the return type with `asserts parameter is Type`:

```js flow-check
function assertString(value: unknown): asserts value is string {
  if (typeof value !== 'string') {
    throw new Error('Expected a string');
  }
}

function uppercase(value: unknown): string {
  assertString(value);
  value as number; // ERROR: value is now a string
  return value.toUpperCase();
}
```

After `assertString(value)` returns, Flow narrows `value` to `string`. You do not need an `if` at the call site.

## Asserting a Condition {#toc-asserting-a-condition}

Use `asserts parameter` without an `is Type` clause to assert that the argument is truthy. You can pass a value directly or an expression that Flow understands as a [refinement](../lang/refinements.md):

```js flow-check
function assert(condition: unknown): asserts condition {
  if (!condition) {
    throw new Error('Assertion failed');
  }
}

function check(name: ?string, count: unknown) {
  assert(name);
  name as string; // OK
  name as null; // ERROR

  assert(typeof count === 'number');
  count as number; // OK
}
```

A truthiness assertion rejects falsy values such as `0`, `false`, and `''`, as well as `null` and `undefined`.

Flow treats `assert(false)` as an abnormal exit, so code after the call is unreachable.

## Validating the Function Body {#toc-validating-the-function-body}

Flow checks that paths where the assertion does not hold exit abnormally, for example by throwing an error. An explicit `return;` or reaching the end of the body is only allowed once the assertion holds. Calling another assertion function can establish the assertion or throw when it fails.

```js flow-check
function missingCheck(value: unknown): asserts value is string {} // ERROR

function incompleteCheck(value: unknown, skip: boolean): asserts value is string {
  if (skip) {
    return; // ERROR: value has not been narrowed
  }
  if (typeof value !== 'string') {
    throw new Error('Expected a string');
  }
}

function missingTruthyCheck(value: ?string): asserts value {} // ERROR
```

When asserting a parameter, the name must refer to a regular parameter of the function, rather than a rest parameter or a name bound by destructuring. For `asserts parameter is Type`, `Type` must be a subtype of the parameter's type. You cannot reassign the asserted parameter to satisfy the assertion: that would change the local parameter, not the caller's value.

Unlike a two-sided type guard, an assertion function does not need to prove that values rejected by the check are outside the asserted type. For example, a function that asserts a `number` may also reject negative numbers.

## Function Types {#toc-function-types-and-aliases}

You can use assertion annotations in function types, including callback parameters, object properties, and [library definitions](../libdefs/creation.md):

```js flow-check
type StringAssertion = (value: unknown) => asserts value is string;

declare function assertString(value: unknown): asserts value is string;

function check(value: unknown, callbackValue: unknown, callback: StringAssertion) {
  const localAssert: StringAssertion = assertString;
  localAssert(value);
  value as string; // OK

  callback(callbackValue);
  callbackValue as string; // OK
}
```

## Call Restrictions {#toc-call-restrictions}

Assertion functions return `void`, so a standalone call is the usual way to use them. Flow applies assertion narrowing to standalone call statements; assigning the result to a variable does not narrow the argument:

```js flow-check
declare function assertString(value: unknown): asserts value is string;

function check(value: unknown) {
  const result = assertString(value);
  result as void; // OK
  value as string; // ERROR: this call did not narrow value

  assertString(value);
  value as string; // OK
}
```

### Asserting the Receiver <SinceVersion version="0.337" /> {#toc-asserting-the-receiver}

Use `asserts this is Type` in a method to narrow the object on which it is called:

```js flow-check
class Message {
  assertText(): asserts this is TextMessage {
    if (!(this instanceof TextMessage)) throw new Error('Expected a text message');
  }
}

class TextMessage extends Message {
  text: string = 'Hello';
}

function read(message: Message): string {
  message.assertText();
  message.text as number; // ERROR: text is a string
  return message.text;
}
```

After `message.assertText()` returns, Flow narrows `message` to `TextMessage`. Calls through `this` or `super` narrow the current `this` value. The same [call restrictions](#toc-call-restrictions) and [annotation requirements](#toc-current-call-limitations) apply.

## Current Limitations {#toc-current-call-limitations}

Flow recognizes function declarations, imported assertion functions, and aliases defined at module scope. Aliases inside a function body need an explicit type annotation for assertion narrowing. The same requirement applies to local objects holding assertion methods:

```js flow-check
declare function assertString(value: unknown): asserts value is string;
type StringAssertion = (value: unknown) => asserts value is string;

function check(value: unknown) {
  const inferred = assertString;
  inferred(value); // ERROR: the local alias needs an annotation

  const annotated: StringAssertion = assertString;
  annotated(value);
  value as string; // OK
}
```

Flow currently requires the callee to be an identifier or a chain of named properties rooted in an identifier, `this`, or `super`, such as `assertString(value)` or `this.checks.assertString(value)`. Computed property access and calling a function returned by another call are not supported for assertion narrowing:

```js flow-check
declare const checks: {
  assertString(value: unknown): asserts value is string,
};
declare function getAssertion(): (value: unknown) => asserts value is string;

function check(value: unknown) {
  checks['assertString'](value); // ERROR
  getAssertion()(value); // ERROR

  checks.assertString(value);
  value as string; // OK
}
```

Flow currently does not narrow values for optional calls.

## Adoption {#toc-adoption}

Assertion functions are available starting in Flow 0.336. In Flow 0.336, enable them in your `.flowconfig`:

```ini
[options]
experimental.assertion_functions=true
```

Starting in Flow 0.337, assertion functions are enabled by default and you do not need this flag.

:::note Migrating `invariant`

Starting in Flow 0.337, Flow no longer gives functions named `invariant` special narrowing or control-flow behavior. If you relied on that behavior, annotate the function or its [library definition](../libdefs/creation.md) with `asserts condition`:

```js flow-check
declare function invariant(condition?: unknown, message?: string): asserts condition;

function check(value: ?string): string {
  invariant(value != null);
  return value; // OK: value is a string
}
```

:::

## See Also {#toc-see-also}

- [Type Guards](./type-guards.md) — narrow values in conditional branches
- [Refinements](../lang/refinements.md) — built-in type narrowing
- [Functions](./functions.md) — function return type annotations
