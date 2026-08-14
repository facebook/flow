---
title: .flowconfig [ignore]
slug: /config/ignore
description: "Configure Flow to ignore files matching project-relative globs when type checking your code."
---

The `[ignore]` section in a `.flowconfig` file tells Flow to ignore files
matching project-relative globs. By default, nothing is ignored.

## Flow 0.328 and later

Things to keep in mind:

1. `*` matches within one directory and `**` matches across directory
   boundaries.
2. Patterns are relative to the project root. Absolute paths and the
   `<PROJECT_ROOT>` placeholder are not supported.
3. Ignores are processed after includes. If you both include and ignore a file,
   it will be ignored.

An example `[ignore]` section might look like:

```
[ignore]
**/__tests__/**
**/src/{foo,bar}/**
**/*.ignore.js
```

This `[ignore]` section will ignore:

1. Any file or directory under a directory named `__tests__`
2. Any file or directory under a directory named `src/foo` or `src/bar`
3. Any file that ends with the extension `.ignore.js`

To match only a directory at the project root, start the pattern with its name:

```
[ignore]
__tests__/**
```

This ignores files under the root `__tests__/` directory, but not files under
other directories named `__tests__/`, such as `src/__tests__/`.

### Exclusions {#toc-ignore-exclusions-globs}
Sometimes you may want to ignore all files inside a directory with the exception of a few. An optional prefix "!" which negates the pattern may help. With this, any matching file excluded by a previous pattern will become included again.

```
[ignore]
node_modules/**
!node_modules/not-ignored-package-A/**
!node_modules/not-ignored-package-B/**
```

## Flow 0.327 and earlier

The following regular-expression syntax works in Flow 0.325 and earlier.

The `[ignore]` section in a `.flowconfig` file tells Flow to ignore files
matching the specified regular expressions when type checking your code. By
default, nothing is ignored.

Things to keep in mind:

1. These are [OCaml regular expressions](https://v2.ocaml.org/api/Str.html#TYPEregexp).
2. These regular expressions match against absolute paths. They probably should
   start with `.*`
3. Ignores are processed AFTER includes. If you both include and ignore a file
   it will be ignored.

An example `[ignore]` section might look like:

```
[ignore]
.*/__tests__/.*
.*/src/\(foo\|bar\)/.*
.*\.ignore\.js
```

This `[ignore]` section will ignore:

1. Any file or directory under a directory named `__tests__`
2. Any file or directory under `.*/src/foo` or under `.*/src/bar`
3. Any file that ends with the extension `.ignore.js`

You may use the `<PROJECT_ROOT>` placeholder in your regular expressions.
At runtime, Flow will treat the placeholder as if it were the absolute
path to the project's root directory. This is useful for writing regular
expressions that are relative rather than absolute.

For example, you can write:

```
[ignore]
<PROJECT_ROOT>/__tests__/.*
```

Which would ignore any file or directory under the directory named `__tests__/`
within the project root. However, unlike the previous example's
`.*/__tests__/.*`, it would NOT ignore files or directories under other
directories named `__tests__/`, like `src/__tests__/`.

### Exclusions {#toc-ignore-exclusions}
Sometimes you may want to ignore all files inside a directory with the exception of a few. An optional prefix "!" which negates the pattern may help. With this, any matching file excluded by a previous pattern will become included again.

```
[ignore]
<PROJECT_ROOT>/node_modules/.*
!<PROJECT_ROOT>/node_modules/not-ignored-package-A/.*
!<PROJECT_ROOT>/node_modules/not-ignored-package-B/.*
```

## See Also {#toc-see-also}

- [`.flowconfig [declarations]`](./declarations.md) — parsing files in declaration mode instead of ignoring them
- [`.flowconfig [untyped]`](./untyped.md) — treating modules as `any` while still resolving imports
