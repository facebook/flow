---
layout: guide
---

It's common for JavaScript code to have introduce "optional" values so that you
have the option of leaving out the value or passing `null` instead.

Using Flow you can use Maybe types for these values. Maybe types work with any
other type by simply prefixing it with a question mark `?` such as `?number` as
a sort of modifier.

Maybe types accept the provided type as well as `null` or `undefined`. So
`?number` would mean `number`, `null`, or `undefined`.

```js
// @flow
function acceptsMaybeNumber(value: ?number) {
  // ...
}

acceptsMaybeNumber(42);        // Works!
acceptsMaybeNumber();          // Works!
acceptsMaybeNumber(undefined); // Works!
acceptsMaybeNumber(null);      // Works!
acceptsMaybeNumber("42");      // Error!
```

## Refining Maybe types <a class="toc" id="toc-refining-maybe-types" href="#toc-refining-maybe-types"></a>

Imagine we have the type `?number`, if we want to use that value as a `number`
we'll need to first check that it is not `null` or `undefined`.

```js
// @flow
function acceptsMaybeNumber(value: ?number) {
  if (value !== null && value !== undefined) {
    return value * 2;
  }
}
```

You can simplify the two checks against `null` and `undefined` using a single
`!= null` check which will do both.

```js
// @flow
function acceptsMaybeNumber(value: ?number) {
  if (value != null) {
    return value * 2;
  }
}
```

You could also flip it around, and check to make sure that the value has a type
of `number` before using it.

```js
// @flow
function acceptsMaybeNumber(value: ?number) {
  if (typeof value === 'number') {
    return value * 2;
  }
}
```
