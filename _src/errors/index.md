---
title: Error Suppressions
slug: /errors
description: "Learn how to suppress Flow's type errors."
---

import {SinceVersion, UntilVersion} from '../../components/VersionTags';

Flow reports many different kinds of errors for many common programming mistakes, but not every JavaScript pattern can be understood by Flow.
If you are confident your code is correct, and that Flow is
erroring too conservatively, you can suppress the error so that
Flow does not report it.

### What is a Suppression? {#toc-what-is-a-suppression}

A suppression is a special kind of comment that you can place on the line before a type
error. It tells Flow not to report that error when checking your code. Suppression
comments look like the following:

```
// <SUPPRESSOR>[<CODE>] extra text
```

A suppressor can be one of the following:
- `$FlowFixMe`: for type errors that you intend to fix later
- `$FlowExpectedError`: for a location where you expect Flow to produce a type error (for instance, when performing an invalid type cast).

<details>
  <summary>Removed suppressors</summary>

- `$FlowIssue` <UntilVersion version="0.280" />: for a type error that you suspect is an issue with Flow
- `$FlowIgnore` <UntilVersion version="0.280" />: for locations where you want Flow to ignore your code
</details>

Note that all of the suppressors behave identically; we simply recommend using them as described here for your own ease of reference.

The `<CODE>` portion of a suppression is required. It specifies which error code the suppression affects.

Some examples of suppression comments:

```
// $FlowFixMe[incompatible-type]

// $FlowExpectedError[incompatible-type]

/* $FlowFixMe[prop-missing] some other text here */

/* $FlowExpectedError[incompatible-type] this
    is a multi-line
    comment */

{ /* $FlowExpectedError[cannot-resolve-name] this is how you suppress errors inside JSX */ }
```

In order to be a valid suppression comment, there are also some conditions that must be true:
- No text can precede the suppressor, or come between the suppressor and the code. For example: `// some text then $FlowFixMe` is not a valid suppression, nor is `// $FlowExpectedError some text [incompatible-type]` or ` //$FlowFixMe [prop-missing]` (note the space here!).
- Suppressions must be on the line immediately before the error they suppress, otherwise they will not apply.

### Making Suppressions More Granular with Error Codes {#toc-making-suppressions-more-granular-with-error-codes}

:::info
Starting from version v0.281, suppressions must have error codes.
:::

Suppressible Flow errors will also have an error code associated with them <SinceVersion version="0.127" />. This code concisely describes the type of issue the error is reporting, and is different between different kinds of errors.

In order to prevent suppressions from suppressing different kinds of type errors on the same line (by default suppressions without codes suppress every error on the following line), you can add an error code to your suppression. For example: `// $FlowFixMe[incompatible-type]` would only suppress errors with the `incompatible-type` code. So:

```js flow-check
// $FlowFixMe[incompatible-type]
3 as string;
```
would report no errors, but
```js flow-check
// $FlowFixMe[prop-missing]
3 as string;
```
would still report a type incompatibility.

To suppress multiple error codes on the same line, you can stack suppression comments one after another, and they will all apply to the first non-comment line like so:

```js flow-check
let y: number | {x : number}  = 1;

// $FlowFixMe[incompatible-type]
// $FlowFixMe[prop-missing]
y.x as string;
```

This will suppress both of the two errors on this line.

### Unsuppressable Errors <SinceVersion version="0.268" /> {#toc-unsuppressable-errors}

Certain kinds of errors can be made unsuppressable. For example, to make `react-rule-hook-naming-convention` and `react-rule-hook-conditional` errors unsuppressable, you can add the following to the `[options]` section in `flowconfig`:

```
unsuppressable_error_codes=react-rule-hook-naming-convention
unsuppressable_error_codes=react-rule-hook-conditional
```
