---
title: Migration from existing patterns
description: "You can migrate from switch statements and conditional expressions to match expressions and statements"
slug: /match/migration
---

## Replacing `switch`

You can turn a `switch` into either a match statement or a match expression, depending on its usage.

If you are using an IDE, you can use the “Refactor `switch` to `match`” refactoring code-action to do most of the below. To activate, the `switch` needs the following properties:
* Every case of the switch must end with a `break`, `return`, or `throw`, except the last case.
* If there is a `default`, it must be the last case.
* The `case` test must be convertible to a match pattern.
* If there is a `let` or `const` in the case body, it must be wrapped in a block.

And the caveats for the resulting match:
* It may contain other `break`s (other than the last one that was removed) that will be a parse error and you will have to figure out what to do with that.
* It may not be exhaustively checked, or identifier patterns may not be valid (e.g. just typed as `string`). You will get new errors you will have to resolve.

### To match statement

Most `switch` statements can be turned into match statements:

* Replace `switch` with `match`
* Delete the `case`
* Replace the colon `:` after the case test with an arrow  `=>`
* Wrap the case body in a block `{ ... }`
* Remove the `break;`
* If multiple cases share a body, use an ["or" pattern](../patterns#or-patterns) `|`
* Replace the `default` with a [wildcard](../patterns#wildcard-patterns) `_`

```js
// Before
switch (action) {
  case 'delete':
  case 'remove':
    data.pop();
    break;
  case 'add':
    data.push(1);
    break;
  default:
    show(data);
}

// After
match (action) {
  'delete' | 'remove' => {
    data.pop();
  }
  'add' => {
    data.push(1);
  }
  _ => {
    show(data);
  }
}
```

If you are depending on the fallthrough behavior of `switch` cases when not using `break` (other than the simple case where the body is completely shared), then you will likely have to refactor your case body code into a function which is called.

### To match expression

If every case of your `switch` has a body that contains a single `return` or a single assignment, then you can turn your `switch` into a match expression.

For the `return` case, make the same changes as in the [to match statement](#to-match-statement) section, except:
* Replace each case body with only the expression being returned, and delete the `return`, and don't use braces for the `case` body
* Separate each case with a comma `,`
* Return the entire match expression

```js flow-check
// Before
function getSizeBefore(imageSize: 'small' | 'medium' | 'large') {
  switch (imageSize) {
    case 'small':
      return 50;
    case 'medium':
      return 100;
    case 'large':
      return 200;
  };
}

// After
function getSizeAfter(imageSize: 'small' | 'medium' | 'large') {
  return match (imageSize) {
    'small' => 50,
    'medium' => 100,
    'large' => 200,
  };
}
```

For the assignment case, make the same changes as in the [to match statement](#to-match-statement) section, except:

* Replace each case body with only the expression being assigned, and don't use braces for the `case` body
* Separate each case with a comma `,`
* Assign the entire match expression to the variable
* If you no longer re-assign the variable, you can change it to a `const`

```js
// Before
let colorSchemeStyles;
switch (colorScheme) {
  case 'darker':
    colorSchemeStyles = colorSchemeDarker;
    break;
  case 'light':
    colorSchemeStyles = colorSchemeLight;
    break;
  case 'unset':
    colorSchemeStyles = colorSchemeDefault;
    break;
}

// After
const colorSchemeStyles = match (colorScheme) {
  'darker' => colorSchemeDarker,
  'light' => colorSchemeLight,
  'unset' => colorSchemeDefault,
};
```

You can replace multiple assignments with a single match expression:
```js
// Before
let color;
let size;
switch (status) {
  case Status.Active:
    color = 'green';
    size = 2;
    break;
  case Status.Paused:
    color = 'yellow';
    size = 1;
    break;
  case Status.Off:
    color = 'red';
    size = 0;
    break;
}

// After (using a tuple):
const [color, size] = match (status) {
  Status.Active => ['green', 2],
  Status.Paused => ['yellow', 1],
  Status.Off => ['red', 0],
};

// After (using an object):
const {color, size} = match (status) {
  Status.Active => {color: 'green', size: 2},
  Status.Paused => {color: 'yellow', size: 1},
  Status.Off => {color: 'red', size: 0},
};
```
Using an object is more verbose, but may be more readable, especially if dealing with more than two variables.

## Replacing conditional ternary expressions

You can replace most conditional expressions `cond ? x : y` with match expressions. This is particularly useful for complex or nested conditional expressions. For example:

```js flow-check
declare const obj:
  | {type: 'a', foo: number}
  | {type: 'b', bar: string}
  | null;

// Before
const a =
  obj === null
    ? 0
    : obj.type === 'a'
      ? obj.foo
      : obj.bar.length;

// After
const b = match (obj) {
  {type: 'a', const foo} => foo,
  {type: 'b', const bar} => bar.length,
  null => 0,
};
```

## Dealing with disjoint object unions

[Disjoint object unions](../../types/unions#toc-disjoint-object-unions) are unions of object types with some distinguishing property. In the following example, that would be the `type` property:

```js flow-check
type Result = {type: 'ok', value: number} | {type: 'error', error: Error};
```

Previous patterns would involve checking the `result.type` property, and then accessing properties off of `result`:

```js flow-check
type Result = {type: 'ok', value: number} | {type: 'error', error: Error};
declare const result: Result;

switch (result.type) {
  case 'ok':
    console.log(result.value);
    break;
  case 'error':
    throw result.error;
}
```

With pattern matching you have to change how you think about it. Rather than doing the conditional checks on the `type` property, you do it on the object itself, and destructure what you need right in the pattern:

```js flow-check
type Result = {type: 'ok', value: number} | {type: 'error', error: Error};
declare const result: Result;

match (result) {
  {type: 'ok', const value} => {
    console.log(value);
  }
  {type: 'error', const error} => {
    throw error;
  }
}
```

If you need to pass in the refined object type itself, you can use an `as` pattern on the object pattern:

```js flow-check
type OK = {type: 'ok', value: number};
type Err = {type: 'error', error: Error}
type Result = OK | Err;

declare const result: Result;

match (result) {
  {type: 'ok', const value} => {
    console.log(value);
  }
  {type: 'error', ...} as err => { // Using `as`
    throw processError(err);
  }
}

declare function processError(err: Err): Error;
```

If you don't need the `type` property included, you could also use an [object rest pattern](../patterns#object-patterns):

```js flow-check
type Result = {type: 'ok', value: number} | {type: 'error', error: Error};

declare const result: Result;

match (result) {
  {type: 'ok', const value} => {
    console.log(value);
  }
  {type: 'error', ...const err} => { // Using object rest
    throw processError(err);
  }
}

declare function processError(err: {error: Error}): Error;
```
