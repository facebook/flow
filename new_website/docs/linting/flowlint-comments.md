---
title: Flowlint Comments
slug: /linting/flowlint-comments
---

Configuring lint settings with `flowlint` comments allows you to specify different
settings within a file and different settings to different regions of different
files. These comments come in three forms:
* [flowlint](#toc-flowlint)
* [flowlint-line](#toc-flowlint-line)
* [flowlint-next-line](#toc-flowlint-next-line)

In all forms, whitespace and asterisks between words are ignored, allowing for flexible formatting.

### flowlint {#toc-flowlint}
The basic `flowlint` comment takes a comma-delimited list of `rule:severity` pairs and
applies those settings for the rest of the source file until overridden. This has
three primary purposes: applying settings over a block, applying settings over a file,
and applying settings over part of a line.

**settings over a block of code:**
A pair of `flowlint` comments can be used to apply a certain setting over a block of code.
For example, to disabling the untyped-type-import lint over a block of type imports would look like this:
```js
import type {
  // flowlint untyped-type-import:off
  Foo,
  Bar,
  Baz,
  // flowlint untyped-type-import:error
} from './untyped.js';
```

**settings over a file:**
A `flowlint` comment doesn't have to have a matching comment to form a block.
An unmatched comment simply applies its settings to the rest of the file. You
could use this, for example, to suppress all sketchy-null-check lints in a particular file:
```js
// flowlint sketchy-null:off
...
```

**settings over part of a line:**
The settings applied by `flowlint` start and end right at the comment itself. This
means that you can do things like
```js
function (a: ?boolean, b: ?boolean) {
  if (/* flowlint sketchy-null-bool:off */a/* flowlint sketchy-null-bool:warn */ && b) {
    ...
  } else {
    ...
  }
}
```
if you want control at an even finer level than you get from the line-based comments.

### flowlint-line {#toc-flowlint-line}
A `flowlint-line` comment works similarly to a `flowlint` comment, except it only
applies its settings to the current line instead of applying them for the rest of the file.
The primary use for `flowlint-line` comments is to suppress a lint on a particular line:
```js
function (x: ?boolean) {
  if (x) { // flowlint-line sketchy-null-bool:off
    ...
  } else {
    ...
  }
}
```


### flowlint-next-line {#toc-flowlint-next-line}
`flowlint-next-line` works the same as `flowlint-line`, except it applies its settings to the next line instead of the current line:
```js
function (x: ?boolean) {
  // flowlint-next-line sketchy-null-bool:off
  if (x) {
    ...
  } else {
    ...
  }
}
```
